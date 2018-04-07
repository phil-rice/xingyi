package one.xingyi.utils.http

import one.xingyi.utils.UtilsSpec

import scala.concurrent.Future
import one.xingyi.utils.functions.AsyncForScalaFuture._
import ImplicitsForTest._
import one.xingyi.utils.language.Language._
class ServiceRequestSpec extends UtilsSpec {

  val serviceRequest = ServiceRequest(Get, Uri("/someUri"), body = Some(Body("someBody")))

  "default to service request  for service request " should "return self" in {
    implicitly[ToServiceRequest[ServiceRequest]].apply(serviceRequest) shouldBe serviceRequest
  }

  "default from serviceRequest  for serviceRequest " should "return self" in {
    implicitly[FromServiceRequest[Future, ServiceRequest]].apply(serviceRequest).await shouldBe serviceRequest
  }

}
