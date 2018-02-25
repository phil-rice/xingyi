package org.validoc.utils.cache

import scala.language.higherKinds

case class CacheStats(size: Int)


trait ShouldCache[Req] extends (Req => Boolean)

trait Cachable[Req] extends (Req => Any)

trait CacheFactory[M[_]] {
  def apply[Res](name: String): Cache[M, Res]
}

trait Cache[M[_], Res] {
  def apply[Req: Cachable](req: Req, raw: Req => M[Res]): M[Res]
  def clear()
  def stats: CacheStats
}

object Cache {

  def apply[M[_], Req, Res](cache: Cache[M, Res], raw: Req => M[Res])(req: Req)(implicit shouldCache: ShouldCache[Req], cachable: Cachable[Req]): M[Res] =
    if (shouldCache(req)) cache(req, raw) else raw(req)

}

