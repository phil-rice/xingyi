package org.validoc.utils.cache

import scala.language.higherKinds

case class CacheStats(size: Int)


trait ShouldCache[Req] extends (Req => Boolean)

trait Cachable[Req] extends (Req => Any)

trait CacheFactory[M[_]] {
  def apply[Req: Cachable, Res](name: String, raw: Req => M[Res]): Cache[M, Req, Res]
}

trait Cache[M[_], Req, Res] extends (Req => M[Res]) {
  def clear()
  def stats: CacheStats
}

object Cache {
  def apply[M[_], Req, Res](cache: Cache[M, Res], raw: Req => M[Res])(req: Req)(implicit shouldCache: ShouldCache[Req], cachable: Cachable[Req]): M[Res] =
    if (shouldCache(req)) cache(req, raw) else raw(req)
}

