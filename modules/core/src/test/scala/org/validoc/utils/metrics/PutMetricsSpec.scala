package org.validoc.utils.metrics

import java.io.{ByteArrayOutputStream, PrintStream}

import org.validoc.utils.UtilsSpec
import org.validoc.utils.strings.Strings

class PutMetricsSpec extends UtilsSpec {


  "NullPutMetrics" should "do nothing" in {
    // ok this is mostly for code coverage. It at least checks that it doesn't die!
    NullPutMetrics(Map("a" -> CountMetricValue))
  }

  "PrintlnPutMetrics" should "Println" in {
    val (_, s) = Strings.recordPrintln(PrintlnPutMetrics(Map("a" -> CountMetricValue)))
    s.trim shouldBe "a -> CountMetricValue"

  }

}
