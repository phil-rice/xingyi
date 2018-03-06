package org.validoc.utils.map

import org.validoc.utils.UtilsSpec

class NoReportMapSizeReporterSpec extends UtilsSpec {

  "NoreportMapSizeReport" should "do nothing" in {
    //OK ... just for coverage...
    NoReportMapSizeReduction.mapSizeChanges(1, Seq())
  }

}
