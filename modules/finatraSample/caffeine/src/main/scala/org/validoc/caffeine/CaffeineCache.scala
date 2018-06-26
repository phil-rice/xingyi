/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.validoc.caffeine

import com.github.blemale.scaffeine.Scaffeine
import org.validoc.utils.cache.{CachableKey, Cache, CacheFactory, ShouldCacheResult}
import org.validoc.utils.functions.MonadConvertor
import org.validoc.utils.local.ExecutionContextWithLocal

import scala.concurrent.Future
import scala.concurrent.duration._
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
  def apply[Req](req: Req)(implicit cachable: CachableKey[Req]) = new CacheKey(req, cachable.id(req))
  def getReqFromKey[Req] = { cacheKey: CacheKey[Req] => cacheKey.req }
}
//Note this ignores the ShouldCacheResult at the moment
object CaffeineCache {

  def defaultCacheBuilder: Scaffeine[Any, Any] =
    Scaffeine()
      //      .recordStats()
      .expireAfterWrite(1 hour)
      .refreshAfterWrite(10.minutes)
      .maximumSize(5000)


  def cacheFactoryForFuture(scaffeine: Scaffeine[Any, Any])(implicit executionContext: ExecutionContextWithLocal) = new CacheFactory[Future] {
    override def apply[Req: CachableKey, Res: ShouldCacheResult](name: String, rawFn: Req => Future[Res]) = new Cache[Future, Req, Res] {
      val cache = scaffeine.buildAsyncFuture[CacheKey[Req], Res](CacheKey.getReqFromKey andThen rawFn)
      override def clear() = cache.underlying.synchronous().invalidateAll()
      override def apply(v1: Req) = cache.get(CacheKey(v1))
      override def raw = rawFn
    }
  }

  def cacheFactory[M[_]](scaffeine: Scaffeine[Any, Any])(implicit executionContext: ExecutionContextWithLocal, toFuture: MonadConvertor[M, Future], fromFuture: MonadConvertor[Future, M]) = new CacheFactory[M] {
    override def apply[Req: CachableKey, Res: ShouldCacheResult](name: String, rawFn: Req => M[Res]) = new Cache[M, Req, Res] {
      val cache = scaffeine.buildAsyncFuture[CacheKey[Req], Res](key => toFuture[Res](raw(key.req)))
      override def apply(req: Req) = fromFuture(cache.get(CacheKey(req)))
      override def clear() = cache.underlying.synchronous().invalidateAll()
      override def raw = rawFn
    }
  }
}
