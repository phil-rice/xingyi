package org.validoc.caffeine

import com.github.blemale.scaffeine.Scaffeine
import org.validoc.utils._
import org.validoc.utils.cache.{Cachable, Cache, CacheFactory}
import org.validoc.utils.functions.CompletableMonad

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.language.higherKinds


//Has a req and a key, but equals delegates to key equals.
class CacheKey[Req](req: Req, val key: Any) {
  override def toString: String = s"CacheKey($key)"
  override def equals(obj: scala.Any): Boolean = obj match {
    case ck: CacheKey[Req] => ck.key == key
    case _ => false
  }
  override def hashCode(): Int = key.hashCode()
}

object CacheKey {
  def apply[Req](req: Req)(implicit cachable: Cachable[Req]) = new CacheKey(req, cachable(req))
}

object CaffeineCache {


  def defaultCacheBuilder[Req, Res](raw: Req => Future[Res]): Scaffeine[Any, Any] =
    Scaffeine()
      .recordStats()
      .expireAfterWrite(1.hour)
      .maximumSize(500)
  //      .buildAsyncFuture(raw)

  def cacheFactoryForFuture(scaffeine: Scaffeine[Any, Any])(implicit executionContext: ExecutionContext) = new CacheFactory[Future] {
    override def apply[Req: Cachable, Res](name: String, rawFn: Req => Future[Res]) = new Cache[Future, Req, Res] {
      val cache = scaffeine.buildAsyncFuture[Req, Res](rawFn)
      override def clear() = cache.underlying.synchronous().invalidateAll()
      override def apply(v1: Req) = cache.get(v1)
      override def raw = rawFn
    }
  }

  def cacheFactory[M[_], H[_]](scaffeine: Scaffeine[Any, Any])(implicit executionContext: ExecutionContext, monad: CompletableMonad[M, H]) = new CacheFactory[M] {
    override def apply[Req: Cachable, Res](name: String, rawFn: Req => M[Res]) = new Cache[M, Req, Res] {
      val cache = scaffeine.buildAsyncFuture[Req, Res] { req =>
        val promise: Promise[Res] = Promise[Res]()
        raw(req).mapTry(promise.tryComplete)
        promise.future
      }
      override def apply(req: Req) = {
        val completable = monad.makePromise[Res]
        cache.get(req).onComplete(monad.complete(completable, _))(executionContext)
        monad.monad(completable)
      }
      override def clear() = cache.underlying.synchronous().invalidateAll()
      override def raw = rawFn
    }
  }
}