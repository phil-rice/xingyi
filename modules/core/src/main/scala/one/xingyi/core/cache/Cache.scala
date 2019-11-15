/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.cache

import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.util.Try


trait CacheKleisli[M[_]] {

  protected def cacheFactory: CacheFactory[M]

  def cache[Req: ClassTag : CachableKey : ShouldUseCache, Res: ClassTag : ShouldCacheResult](name: String)(raw: Req => M[Res]): Req => M[Res] =
    cacheFactory.apply[Req, Res](name, raw)

}

case class CacheStats(size: Int)


trait ShouldUseCache[Req] extends (Req => Boolean)

object ShouldUseCache {
  implicit def shouldCache[Req]: ShouldUseCache[Req] = _ => true
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

  def cacheForName[Req, Res](name: String): Option[Cache[M, Req, Res]]

  def entries: Iterable[(String, CachingInfoAndOps)]
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

