/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.logging

import one.xingyi.core.UtilsSpec
import one.xingyi.core.strings.Strings

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
