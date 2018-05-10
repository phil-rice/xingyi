package one.xingyi.core.map

import one.xingyi.core.UtilsSpec

class NoReportMapSizeReporterSpec extends UtilsSpec {

  "NoreportMapSizeReport" should "do nothing" in {
    //OK ... just for coverage...
    NoReportMapSizeReduction.mapSizeChanges(1, Seq())
  }

}
