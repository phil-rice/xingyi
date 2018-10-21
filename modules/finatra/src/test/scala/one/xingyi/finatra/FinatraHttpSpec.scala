package one.xingyi.finatra
import com.twitter.finagle.http.{HeaderMap, Request, Response, Status}
import com.twitter.finatra.http.HttpTest
import one.xingyi.core.http.{ToServiceRequest => _, _}
import one.xingyi.finatra.FinatraImplicits._
import ImplicitsForTest._
import org.mockito.Mockito._
import com.twitter.util.{Future => TFuture}
import one.xingyi.core.strings.Strings

class ResponseToServiceResponseSpec extends AbstractToServiceResponseSpec[Response] {

  override def makeT: Response = {
    val response = spy(classOf[Response])
    doReturn(Status(200)).when(response).status
    doReturn("someBody").when(response).contentString
    val headerMap = new HeaderMap {
      override def getAll(key: String): Seq[String] = ???
      override def get(key: String): Option[String] = ???
      override def add(k: String, v: String): HeaderMap = ???
      override def set(k: String, v: String): HeaderMap = ???
      override def +=(kv: (String, String)): this.type = ???
      override def -=(key: String): this.type = ???
      override def iterator: Iterator[(String, String)] = List("header1" -> "value1").iterator
    }
    doReturn(headerMap).when(response).headerMap
    response
  }
}


trait RequestFixture {
  implicit class RequestOps(r: Request) {
    def addHeaders(tuples: (String, String)*): Request = {tuples.foreach { case (n, v) => r.headerMap.add(n, v) }; r}
    def addBody(s: String): Request = {r.contentString = s; r}


  }
}
class RequestToServiceRequestSpec extends AbstractToServiceRequestSpec[Request] with RequestFixture {
  override def makeWithBody: Request = Request("http://someHost:1111/somePath?a=1&b=2").addHeaders("one" -> "valueOne").addBody("someBody")
  override def makeWithoutBody: Request = Request("http://someHost:1111/somePath?a=1&b=2").addHeaders("one" -> "valueOne")

}


class RequestAbstractFromServiceRequestSpec extends AbstractFromServiceRequestSpec[TFuture, Request] with RequestFixture {
  override def checkMethod(res: Request, sr: ServiceRequest): Unit = res.method.name.toLowerCase shouldBe sr.method.toString.toLowerCase
  override def checkHeaders(res: Request, sr: ServiceRequest): Unit = res.headerMap.map { case (n, v) => Header(n, v) }.toList shouldBe sr.headers
  override def checkBody(res: Request, sr: ServiceRequest): Unit = Strings.toOption(res.contentString) shouldBe sr.body.map(_.s)
  override def checkUri(res: Request, sr: ServiceRequest): Unit = res.uri shouldBe sr.uri.asUriString
}
