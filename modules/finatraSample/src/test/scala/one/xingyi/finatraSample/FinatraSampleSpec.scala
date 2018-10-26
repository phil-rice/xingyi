package one.xingyi.finatraSample

import com.twitter.util.{Await, FuturePool}
import one.xingyi.core.UtilsSpec
import one.xingyi.core.http._
import one.xingyi.core.logging.PrintlnLoggingAdapter
import one.xingyi.finatra.AsyncForTwitterFuture

import scala.util.Success

class FinatraSampleSpec extends UtilsSpec {
  val serviceRequest = ServiceRequest(Get, Uri("/someUri"), Seq(), Some(Body("theBody")))
  val serviceResponse = ServiceResponse(Status(200), Body("response: theBody"), List(ContentType("text/html")))

  behavior of "FinatraSample"

  lazy val setup = new FinatraPromotionSetup()(FuturePool.immediatePool)
  it should "have somethings setup" in {
    setup.monad should be(a[AsyncForTwitterFuture])
    setup.loggingAdapter shouldBe PrintlnLoggingAdapter
    capturePrintln(    setup.logRequestAndResult.apply[ServiceRequest, ServiceResponse](this, "someMessagePrefix").apply(serviceRequest, Success(Right( serviceResponse))))._2.trim shouldBe
    """[DEBUG] success.success:ServiceRequest(Get,None,Path(/someUri),WrappedArray(),List(),Some(Body(theBody))),ServiceRequest(Get,None,Path(/someUri),WrappedArray(),List(),Some(Body(theBody))),ServiceResponse(Status(200),Body(response: theBody),List(ContentType(text/html))),ServiceResponse(Status(200),Body(response: theBody),List(ContentType(text/html)))"""
    Await.result(setup.httpFactory.apply(ServiceName("unused"))(serviceRequest)) shouldBe serviceResponse

  }

}
