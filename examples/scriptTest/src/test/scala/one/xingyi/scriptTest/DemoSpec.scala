package one.xingyi.scriptTest
import one.xingyi.core.UtilsSpec
import one.xingyi.core.simpleServer.SimpleHttpServer
import one.xingyi.scriptBackend1.Backend1
import one.xingyi.scriptBackend2.Backend2
import one.xingyi.scriptBackend3.Backend3
import one.xingyi.scriptWebsite.WebsiteApp
import org.openqa.selenium.chrome.ChromeDriver
import org.scalatest.concurrent.Eventually
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Millis, Span}
import org.scalatest.{BeforeAndAfter, FlatSpec}
import org.scalatestplus.selenium.WebBrowser

abstract class DemoSpec[P] extends FlatSpec with Matchers with BeforeAndAfter with Eventually with UtilsSpec with WebBrowser {

  //  webDriver.setJavascriptEnabled(true)
  val host = "http://localhost:9000"
  implicit val webDriver = {
    if (System.getenv("travis") == null) {
      import org.openqa.selenium.chrome.ChromeOptions
      val chromeDriverPath = "./Chromedriver.exe"
      System.setProperty("webdriver.chrome.driver", chromeDriverPath)
      val options = new ChromeOptions
      options.addArguments("--headless", "--disable-gpu", "--window-size=1920,1200", "--ignore-certificate-errors")
      new ChromeDriver(options);
    } else null
  }

  override implicit val patienceConfig = PatienceConfig(timeout = scaled(Span(10000, Millis)), interval = scaled(Span(1000, Millis)))

  val websiteApp = new WebsiteApp
  var websiteServer: SimpleHttpServer = null

  def backendName: String = getClass.getSimpleName

  def startBackend()
  def stopBackend()


  behavior of s"Website with backend $backendName"

  it should "test we can actually read a person from the backend" in {
    if (webDriver != null) {
      println("in test")
      goTo(host + "/person/someName/edit")
      //    println("page source")
      //    println(pageSource)
      pageTitle shouldBe "Xing Yi demo"
      textField("name").value shouldBe "someName"
      //    textField("line1").value shouldBe "line1"
      //    textField("line2").value shouldBe "line2"

      textField("line1").value = "newLine1"
      textField("line2").value = "newLine2"
      submit
      //    Thread.sleep(2000)
      goTo(host + "/person/someName/edit")
      eventually {
        textField("name").value shouldBe "someName"
        textField("line1").value shouldBe "newLine1"
        textField("line2").value shouldBe "newLine2"

      }
    }
  }


  before {
    if (webDriver != null)
      eventually { // there are multiple backends
        // /websites that need to bind, so wihtout this we get non deterministic failures
        println(s"Attempting to start $backendName website")
        websiteServer = websiteApp.cheapServer.start
        println(s"Attempting to start $backendName backed")
        startBackend()
        println(s"Succeeded in starting both for $backendName")
      }
  }


  after {
    if (webDriver != null) {
      println(s"starting to close websiteApp for $backendName")
      websiteApp.cheapServer.stop(websiteServer)
      println(s"starting to close backend for $backendName")
      stopBackend()
      println(s"closed everything $backendName")
    }
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
