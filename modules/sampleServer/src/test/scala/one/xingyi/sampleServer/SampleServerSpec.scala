/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.sampleServer
import one.xingyi.core.UtilsSpec
import one.xingyi.core.cache.DurationStaleCacheStategy
import one.xingyi.core.http._
import one.xingyi.core.logging.{LogRequestAndResult, LoggingAdapter, SimpleLogRequestAndResult}
import one.xingyi.core.map.NoMapSizeStrategy
import one.xingyi.core.metrics.PutMetrics
import org.scalatest.BeforeAndAfterAll

class SampleServerSpec extends UtilsSpec with BeforeAndAfterAll{


  behavior of "SampleServer"

  val server = new SampleServer(10000)
  override protected def afterAll(): Unit = {
    server.server.stop()
    super.afterAll()
  }

  import server._
  it should "have an httpfactory that is faked up" in {
    await(httpFactory(ServiceName("someServiceName")) apply ServiceRequest(Method("get"), Uri("/someUri"), body = Some(Body("someBody")))) shouldBe ServiceResponse(Status(200), Body("response: someBody"), ContentType("text/html"))
  }

  it should "have a println logging adapter" in {
    implicitly[LoggingAdapter] shouldBe server.loggingAdapter
  }

  it should "have a resource bundle using 'messages' " in {
    resourceBundle.getString("some.message") shouldBe "the message"
  }

  it should "have a println put metrics" in {
    implicitly[PutMetrics] shouldBe server.putMetrics
  }
  it should "have a SimpleLogRequestAndResult " in {
    implicitly[LogRequestAndResult[Throwable]] shouldBe logRequestAndResult
    logRequestAndResult.asInstanceOf[SimpleLogRequestAndResult[_]].loggingAdapter shouldBe server.loggingAdapter
  }
  it should "have a caching service factory" in {
    cacheFactory.sizeStrategy shouldBe NoMapSizeStrategy
    cacheFactory.cachingStrategy shouldBe DurationStaleCacheStategy(10000000000L, 10000000000000L)
  }



}
