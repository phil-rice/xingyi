package one.xingyi.utils.logging

import java.util.ResourceBundle

import one.xingyi.utils.UtilsSpec
import org.mockito.Mockito._

class LogRequestAndResultForBundleSpec extends UtilsSpec {


  behavior of "LogRequestAndResultForBundle"

  it should "get it's messages from an implicit bundle" in {
    implicit val bundler =ResourceBundle.getBundle("testmessages")
    implicit val adapter = mock[LoggingAdapter]
    val requester = new LogRequestAndResultForBundle[String]
//    when(bundler.getString("messagePrefix.messagePostfix")) thenReturn "someMessage {0} {1}"
    requester.format("messagePrefix", "messagePostfix")("one", "two") shouldBe "someMessage one two"
  }
}
