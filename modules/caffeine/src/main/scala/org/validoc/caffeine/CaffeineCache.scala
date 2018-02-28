package org.validoc.caffeine

import com.github.blemale.scaffeine.Scaffeine
import org.validoc.utils.cache.{Cachable, Cache, CacheFactory}
import org.validoc.utils.functions.MonadConvertor
import org.validoc.utils.local.ExecutionContextWithLocal

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.language.{higherKinds, postfixOps}

//Has a req and a key, but equals delegates to key equals.
class CacheKey[Req](val req: Req, val key: Any) {
  override def toString: String = s"CacheKey($key)"
  override def equals(obj: scala.Any): Boolean = obj match {
    case ck: CacheKey[Req] => ck.key == key
    case _ => false
  }
  override def hashCode(): Int = key.hashCode()
}

object CacheKey {
  def apply[Req](req: Req)(implicit cachable: Cachable[Req]) = new CacheKey(req, cachable(req))
  def getReqFromKey[Req] = { cacheKey: CacheKey[Req] => cacheKey.req }
}

object CaffeineCache {

  def defaultCacheBuilder: Scaffeine[Any, Any] =
    Scaffeine()
      //      .recordStats()
      .expireAfterWrite(1 hour)
      .refreshAfterWrite(10.minutes)
      .maximumSize(5000)


  def cacheFactoryForFuture(scaffeine: Scaffeine[Any, Any])(implicit executionContext: ExecutionContextWithLocal) = new CacheFactory[Future] {
    override def apply[Req: Cachable, Res](name: String, rawFn: Req => Future[Res]) = new Cache[Future, Req, Res] {
      val cache = scaffeine.buildAsyncFuture[CacheKey[Req], Res](CacheKey.getReqFromKey andThen rawFn)
      override def clear() = cache.underlying.synchronous().invalidateAll()
      override def apply(v1: Req) = cache.get(CacheKey(v1))
      override def raw = rawFn
    }
  }

  def cacheFactory[M[_]](scaffeine: Scaffeine[Any, Any])(implicit executionContext: ExecutionContextWithLocal, toFuture: MonadConvertor[M, Future], fromFuture: MonadConvertor[Future, M]) = new CacheFactory[M] {
    override def apply[Req: Cachable, Res](name: String, rawFn: Req => M[Res]) = new Cache[M, Req, Res] {
      val cache = scaffeine.buildAsyncFuture[CacheKey[Req], Res](key => toFuture[Res](raw(key.req)))
      override def apply(req: Req) = fromFuture(cache.get(CacheKey(req)))
      override def clear() = cache.underlying.synchronous().invalidateAll()
      override def raw = rawFn
    }
  }
}