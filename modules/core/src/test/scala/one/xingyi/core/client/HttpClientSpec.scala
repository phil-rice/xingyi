package one.xingyi.core.client

import java.net.HttpURLConnection

import one.xingyi.core.UtilsSpec
import one.xingyi.core.http.{Get, ServiceRequest, Uri}
import org.mockito.Mockito._
//TODO This is thoroughly tested elsewhere. I should probably add unit tests though

class HttpClientSpec extends UtilsSpec {

  behavior of "HttpClient"


  it should "report exceptions while connecting as  UrlDidntResponse" in {
    val serviceRequest = ServiceRequest(Get, Uri("http://doesnt.exist.com/"))
    val connection = mock[HttpURLConnection]
    when(connection.connect()) thenThrow new RuntimeException
    intercept[UrlDidntResponse](HttpClient.connect(serviceRequest, connection))
  }
}
