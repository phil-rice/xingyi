/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.metrics

import one.xingyi.core.UtilsSpec

import scala.util.{Failure, Success}

class ReportDataSpec extends UtilsSpec {

  behavior of "Default Report Data"

  val rd = implicitly[ReportData[ String]]
  it should "pass the duration and a count of success when the result was successful" in {
    rd("prefix", 1000)(Success(Right("xxx"))) shouldBe Map("prefix.success" -> CountMetricValue, "prefix.duration" -> HistogramMetricValue(1000))
  }
  it should "pass the duration and a count of failed when the result was failed" in {
    rd("prefix", 1000)(Success(Left("xxx"))) shouldBe Map("prefix.failed" -> CountMetricValue, "prefix.duration" -> HistogramMetricValue(1000))
  }
  it should "pass the duration and a count of exception when the result was an exception" in {
    rd("prefix", 1000)(Failure(new RuntimeException)) shouldBe Map("prefix.exception" -> CountMetricValue, "prefix.duration" -> HistogramMetricValue(1000))
  }
}
