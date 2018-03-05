package org.validoc.utils.metrics

import java.io.{ByteArrayOutputStream, PrintStream}

import org.validoc.utils.UtilsSpec

class PutMetricsSpec extends UtilsSpec {


  "NullPutMetrics" should "do nothing" in {
    // ok this is mostly for code coverage. It at least checks that it doesn't die!
    NullPutMetrics(Map("a"-> CountMetricValue))
  }

  "PrintlnPutMetrics" should "Println" in {
    val bytes = new ByteArrayOutputStream()

    Console.withOut(new PrintStream(bytes)){
      PrintlnPutMetrics(Map("a"-> CountMetricValue))
    }
    bytes.toString("UTF-8").trim shouldBe "a -> CountMetricValue"

  }

}
