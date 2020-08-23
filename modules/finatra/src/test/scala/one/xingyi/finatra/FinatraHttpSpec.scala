/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.finatra
import com.twitter.finagle.http.{HeaderMap, Request, Response, Status}
import com.twitter.util.{Future => TFuture}
import one.xingyi.core.CoreSpec
import one.xingyi.core.http.{ToServiceRequest => _, _}
import one.xingyi.core.strings.Strings
import one.xingyi.finatra.FinatraImplicits.ImplicitsForTest._
import one.xingyi.finatra.FinatraImplicits._
import org.mockito.Mockito._

class ResponseToServiceResponseSpec extends AbstractToServiceResponseSpec[String, Response] with CoreSpec {
  override def makeReq: String = "some ignored parameter"

  override def makeT: Response = {
    val response = spy(classOf[Response])
    doReturn(Status(200)).when(response).status
    doReturn("someBody").when(response).contentString
    val headerMap = new HeaderMap {
      override def getAll(key: String): Seq[String] = ???
      override def get(key: String): Option[String] = ???
      override def add(k: String, v: String): HeaderMap = ???
      override def set(k: String, v: String): this.type = ???
      override def +=(kv: (String, String)): this.type = ???
      override def -=(key: String): this.type = ???
      override def iterator: Iterator[(String, String)] = List("header1" -> "value1").iterator
      override def addUnsafe(k: String, v: String): HeaderMap = ???
      override def setUnsafe(k: String, v: String): this.type = ???
      override def removeHeader(name: String): this.type = ???
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
