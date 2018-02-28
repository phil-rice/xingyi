package org.validoc.utils.cache

import org.validoc.utils.functions.{CompletableMonad, MonadWithException}

import scala.language.higherKinds

case class CacheStats(size: Int)


trait ShouldCache[Req] extends (Req => Boolean)

object ShouldCache{
  implicit  def shouldCache[Req] = new ShouldCache[Req] {
    override def apply(v1: Req): Boolean = true
  }
}

trait Cachable[Req] extends (Req => Any)
object Cachable{
  implicit def defaultIsAll[Req] = new Cachable[Req] {
    override def apply(v1: Req): Any = v1
  }
}

trait CacheFactory[M[_]] {
  def apply[Req: Cachable, Res](name: String, raw: Req => M[Res]): Cache[M, Req, Res]
}

trait Cache[M[_], Req, Res] extends (Req => M[Res]) {
  def raw: Req => M[Res]
  def clear()
}

object Cache {
  def apply[M[_], Req, Res](cache: Cache[M, Req, Res])(implicit shouldCache: ShouldCache[Req], cachable: Cachable[Req]): Req => M[Res] = { req =>
    if (shouldCache(req)) cache(req) else cache.raw(req)
  }
}

