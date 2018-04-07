package one.xingyi.utils.map

import one.xingyi.utils.UtilsSpec

class NoReportMapSizeReporterSpec extends UtilsSpec {

  "NoreportMapSizeReport" should "do nothing" in {
    //OK ... just for coverage...
    NoReportMapSizeReduction.mapSizeChanges(1, Seq())
  }

}
