package one.xingyi.finatra

import com.twitter.finagle.http.Status
import com.twitter.finatra.http.EmbeddedHttpServer
import com.twitter.inject.server.FeatureTest
import one.xingyi.core.UtilsSpec

class PingControllerSpec extends FeatureTest {

  override val server = new EmbeddedHttpServer(new FinatraServer(9004, new PingController))

  test("ExampleServer#perform feature") {
    server.httpGet(
      path = "/ping",
      andExpect = Status.Ok,
      withBody = "pong")
  }

}
