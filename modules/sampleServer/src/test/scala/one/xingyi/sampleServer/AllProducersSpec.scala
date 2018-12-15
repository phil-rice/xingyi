/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.sampleServer
import one.xingyi.core.UtilsSpec
import one.xingyi.core.cache.{CacheFactory, CachingServiceFactory, DurationStaleCacheStategy}
import one.xingyi.core.http._
import one.xingyi.core.logging.{LogRequestAndResult, LoggingAdapter, SimpleLogRequestAndResult}
import one.xingyi.core.map.NoMapSizeStrategy
import one.xingyi.core.metrics.PutMetrics
import one.xingyi.core.monad.IdentityMonad
import org.json4s.JsonAST.JValue
import org.scalatest.BeforeAndAfterAll
import one.xingyi.json4s.Json4sParser._
import one.xingyi.json4s.Json4sWriter._
import one.xingyi.core.monad.AsyncForScalaFuture._

import scala.concurrent.{Await, Future}
import scala.util.Success

class AllProducersSpec extends UtilsSpec with AllProducersSetup {
  //implicit val cacheFactory = mock[CachingServiceFactory[Future]]
  val serviceRequest = ServiceRequest(Get, Uri("/someUri"), Seq(), Some(Body("theBody")))
  val serviceResponse = ServiceResponse(Status(200), Body("response: theBody"), List(ContentType("text/html")))

  behavior of "AllProducers"

  it should "have a smoke test that just creates it and checks it doesn't throw an exception" in {
    val setup = new AllProducers[Future, JValue, Throwable](1000)
    capturePrintln(setup.logReqAndResult.apply[ServiceRequest, ServiceResponse](this, "somePrefix") apply(serviceRequest, Success(Right(serviceResponse))))._2.trim shouldBe
    "[DEBUG] success.success:ServiceRequest(Get,None,Path(/someUri),WrappedArray(),List(),Some(Body(theBody))),ServiceRequest(Get,None,Path(/someUri),WrappedArray(),List(),Some(Body(theBody))),ServiceResponse(Status(200),Body(response: theBody),List(ContentType(text/html))),ServiceResponse(Status(200),Body(response: theBody),List(ContentType(text/html)))"

    await(setup.httpFactory.apply(ServiceName("unused"))(serviceRequest)) shouldBe serviceResponse

  }

}
