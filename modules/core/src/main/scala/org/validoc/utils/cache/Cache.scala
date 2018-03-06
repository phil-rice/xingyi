package org.validoc.utils.cache

import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.util.Try


trait CacheKleisli[M[_]] {

  protected def cacheFactory: CacheFactory[M]
  def cache[Req: ClassTag : CachableKey : ShouldUseCache, Res: ClassTag : ShouldCacheResult](name: String)(raw: Req => M[Res]): Req => M[Res] =
    Cache[M, Req, Res](cacheFactory[Req, Res](name, raw))

}
case class CacheStats(size: Int)


trait ShouldUseCache[Req] extends (Req => Boolean)

object ShouldUseCache {
  implicit def shouldCache[Req] = new ShouldUseCache[Req] {
    override def apply(v1: Req): Boolean = true
  }
}

trait ShouldCacheResult[Res] {
  def shouldCacheStrategy(req: Try[Res]): Boolean
}

trait ShouldCacheResultUsingSucesses[Res] extends ShouldCacheResult[Res] {
  def shouldCacheStrategy(req: Try[Res]): Boolean = req.isSuccess
}

object ShouldCacheResult {

  implicit object ShouldCacheResultForString$ extends ShouldCacheResultUsingSucesses[String]

}


trait CacheFactory[M[_]] {
  def apply[Req: CachableKey, Res: ShouldCacheResult](name: String, raw: Req => M[Res]): Cache[M, Req, Res]
}


trait Cache[M[_], Req, Res] extends (Req => M[Res]) {
  def raw: Req => M[Res]
  def clear()
}

object Cache {
  def apply[M[_], Req, Res](cache: Cache[M, Req, Res])(implicit shouldCache: ShouldUseCache[Req], cachable: CachableKey[Req]): Req => M[Res] = { req =>
    if (shouldCache(req)) cache(req) else cache.raw(req)
  }
}

