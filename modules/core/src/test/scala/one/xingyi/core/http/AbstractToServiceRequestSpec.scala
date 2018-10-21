package one.xingyi.core.http
import one.xingyi.core.{UtilsSpec, http}

import scala.reflect.ClassTag

abstract class AbstractToServiceRequestSpec[T: ClassTag](implicit toServiceRequest: ToServiceRequest[T]) extends UtilsSpec {

  def makeWithBody: T
  def makeWithoutBody: T
  val domain = Domain(Protocol("http"), HostName("someHost"), Port(1111))
  val path = Path("/somePath")
  val headers = List(Header("one", "valueOne"))

  behavior of s"toServiceResquest for ${implicitly[ClassTag[T]].runtimeClass.getName}"

  //  case class ServiceRequest(method: Method, domain: Option[Domain], path: Path, params: Seq[QueryParam], headers: Seq[Header], body: Option[Body]) {

  it should "turn a T to a ServiceRequest" in {
    toServiceRequest(makeWithBody) shouldBe ServiceRequest(Get, Some(domain), path, params = QueryParam("?a=1&b=2"), headers = headers, Some(Body("someBody")))
    toServiceRequest(makeWithoutBody) shouldBe ServiceRequest(Get, Some(domain), path, params = QueryParam("?a=1&b=2"), headers = headers, None)
  }

}
