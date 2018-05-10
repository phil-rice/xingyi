package one.xingyi.core.http

import one.xingyi.core.UtilsSpec

class PathSpec extends UtilsSpec {

  behavior of "Path"

  it should "start with a /" in {
    Path("/someUri")
    intercept[IllegalArgumentException](Path("someUri"))
  }

  it should "dump the path as the asUriFragment" in {
    Path("/some/uri").asUriString shouldBe "/some/uri"
  }

}
