package org.validoc.caffeine

import java.util.concurrent.{CompletionStage, Executor, TimeUnit}

import com.github.benmanes.caffeine.cache.{AsyncCacheLoader, AsyncLoadingCache, CacheLoader, Caffeine}
import org.validoc.utils.cache.{Cachable, Cache, CacheFactory, CacheStats}
import org.validoc.utils._

import scala.concurrent.Future
import scala.language.higherKinds


//Has a req and a key, but equals delegates to key equals.
class CacheKey[Req](req: Req, key: Any) {
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
  def toScala[T](cs: CompletionStage[T]): Future[T] = {
    cs match {
      case cf: CF[T] => cf.wrapped
      case _ =>
        val p = new P[T](cs)
        cs whenComplete p
        p.future
    }
  }
  def cache[M[_], Req, Res](loader: AsyncCacheLoader[CacheKey[Req], Res])(implicit executor: Executor): AsyncLoadingCache[CacheKey[Req], Res] =
    Caffeine.newBuilder()
      .maximumSize(10000)
      .expireAfterWrite(10, TimeUnit.MINUTES)
      .buildAsync[CacheKey[Req], Res](loader)

  import FutureConverters ._
  implicit def cacheFactory[M[_]] = new CacheFactory[M[_]] {
    override def apply[Req: Cachable, Res](name: String, raw: Req => M[Res]): Cache[M, Req, Res] = new Cache[M, Req, Res] {
      val c = cache[M, ReqRes](raw =>
      )
      override def clear(): Unit = ???
      override def stats: CacheStats = ???
      override def apply(v1: Req): M[Res] = ???
    }
  }

}
