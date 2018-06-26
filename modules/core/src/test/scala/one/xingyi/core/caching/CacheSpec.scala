/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.caching

import java.util.concurrent.atomic.AtomicInteger

import one.xingyi.core.UtilsSpec
import one.xingyi.core.cache.{Cache, ShouldUseCache}

import scala.concurrent.Future
import one.xingyi.core.monad.AsyncForScalaFuture._
import ImplicitsForTest._
import one.xingyi.core.language.Language._

class CacheSpec extends UtilsSpec {

  behavior of "Cache object"

  it should "use the cached value unless should use cache is false " in {
    val cache = new Cache[Future, String, String] {
      override def raw = {v1 => Future.successful(v1 + "/raw/" + int.incrementAndGet())}
      val int = new AtomicInteger()
      override def clear() {}
      override def apply(v1: String) =Future.successful(v1 + "/apply/" + int.incrementAndGet())
    }
    //default is 'should cache'
    //    val fn =  _
    (Cache[Future, String, String](cache) apply ("123")).await() shouldBe "123/apply/1"
    (Cache[Future, String, String](cache) apply ("123")).await() shouldBe "123/apply/2"
    (Cache[Future, String, String](cache) apply ("123")).await() shouldBe "123/apply/3"

    implicit object ShouldUseCacheForStringTest extends ShouldUseCache[String] {
      override def apply(v1: String) = false
    }
    (Cache[Future, String, String](cache) apply ("123")).await() shouldBe "123/raw/4"
    (Cache[Future, String, String](cache) apply ("123")).await() shouldBe "123/raw/5"
    (Cache[Future, String, String](cache) apply ("123")).await() shouldBe "123/raw/6"

  }

}
