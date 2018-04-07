package one.xingyi.utils.logging

import one.xingyi.utils.UtilsSpec
import one.xingyi.utils.strings.Strings

class LoggingAdapterSpec extends UtilsSpec {


  "NullLoggingAdapter" should "do nothing" in {
    Strings.recordPrintln(NullLoggingAdapter.info("sender")("message"))._2 shouldBe ""
    Strings.recordPrintln(NullLoggingAdapter.debug("sender")("message"))._2 shouldBe ""
    Strings.recordPrintln(NullLoggingAdapter.trace("sender")("message"))._2 shouldBe ""
    Strings.recordPrintln(NullLoggingAdapter.error("sender")("message", new RuntimeException("somemessage")))._2 shouldBe ""

  }
  "PrintlnLoggingAdapter" should "print" in {
    Strings.recordPrintln(PrintlnLoggingAdapter.info("sender")("message"))._2.trim shouldBe "[INFO] message"
    Strings.recordPrintln(PrintlnLoggingAdapter.debug("sender")("message"))._2.trim shouldBe "[DEBUG] message"
    Strings.recordPrintln(PrintlnLoggingAdapter.trace("sender")("message"))._2.trim shouldBe "[TRACE] message"
    Strings.recordPrintln(PrintlnLoggingAdapter.error("sender")("message", new RuntimeException("somemessage")))._2.trim should startWith ("[ERROR]message   --  RuntimeException somemessage")
  }
}
