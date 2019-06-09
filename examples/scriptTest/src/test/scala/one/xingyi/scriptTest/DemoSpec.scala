package one.xingyi.scriptTest
import com.sun.net.httpserver.HttpServer
import one.xingyi.core.UtilsSpec
import one.xingyi.core.http.{Get, ServiceRequest, Uri}
import one.xingyi.core.simpleServer.SimpleHttpServer
import one.xingyi.scriptBackend1.{Backend1, Person}
import one.xingyi.scriptBackend2.Backend2
import one.xingyi.scriptBackend3.Backend3
import one.xingyi.scriptWebsite.{Website, WebsiteApp}
import org.scalatest.concurrent.Eventually
import org.scalatest.selenium.HtmlUnit
import org.scalatest.time.{Millis, Span}
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, FlatSpec, Matchers}

abstract class DemoSpec[P] extends FlatSpec with Matchers with BeforeAndAfter with Eventually with UtilsSpec with HtmlUnit {


  val host = "http://localhost:9000/"

  override implicit val patienceConfig = PatienceConfig(timeout = scaled(Span(10000, Millis)), interval = scaled(Span(1000, Millis)))
  val websiteApp = new WebsiteApp
  var websiteServer: SimpleHttpServer = null

  def backendName: String = getClass.getSimpleName

  def startBackend()
  def stopBackend()

  def pToNameLine1Line2(any: Any): (String, String, String) = ???

  behavior of s"Website with backend $backendName"

  it should "test we can actually read a person from the backend" in {
    println("in test")
//    goTo(host + "/person/someName/edit")
//    pageTitle shouldBe "Xing Yi Demo"
//    textField("name").value shouldBe "someName"
//    textField("line1").value shouldBe "line1"
//    textField("line2").value shouldBe "line2"
//
//    textField("line1").value = "newLine1"
//    textField("line2").value = "newLine2"
//    submit
//    goTo(host + "/person/someName/edit")
//    eventually {
//      textField("name").value shouldBe "someName"
//      textField("line1").value shouldBe "line1"
//      textField("line2").value shouldBe "line2"
//
//    }
  }


  before {
    eventually { // there are multiple backends
      // /websites that need to bind, so wihtout this we get non deterministic failures
      println(s"Attempting to start $backendName website")
      websiteServer = websiteApp.cheapServer.start
      println(s"Attempting to start $backendName backed")
      startBackend()
    }
    println(s"Succeeded in starting both for $backendName")
  }


  after {
    println(s"starting to close websiteApp for $backendName")
    websiteApp.cheapServer.stop(websiteServer)
    println(s"starting to close backend for $backendName")
    stopBackend()
    println(s"closed everything $backendName")
  }
}

class DemoBackend1Spec extends DemoSpec {
  val backend = new Backend1()
  var server: SimpleHttpServer = null
  override def startBackend(): Unit = {
    server = backend.backend.start
  }
  override def stopBackend(): Unit = {
    backend.backend.stop(server)
  }

}
class DemoBackend2Spec extends DemoSpec {
  val backend = new Backend2()
  var server: SimpleHttpServer = null
  override def startBackend(): Unit = {
    server = backend.backend.start
  }
  override def stopBackend(): Unit = {
    backend.backend.stop(server)
  }

}
class DemoBackend3Spec extends DemoSpec {
  val backend = new Backend3()
  var server: SimpleHttpServer = null
  override def startBackend(): Unit = {
    server = backend.backend.start
  }
  override def stopBackend(): Unit = {
    backend.backend.stop(server)
  }

}
