package org.validoc.utils.http

import org.validoc.utils.UtilsSpec


class UriTest extends UtilsSpec {
  behavior of "Uri"

  it should "encode the data correctly" in {
    Uri(Protocol("http"), HostName("someHost:80"), Path("/somePath")).asUriString shouldBe "http://someHost:80/somePath"
    Uri(Protocol("http"), HostName("someHost:80"), Path("/somePath"), QueryParam(("a", "1")):_*).asUriString shouldBe
      "http://someHost:80/somePath?a=1"
    Uri(Protocol("http"), HostName("someHost:80"), Path("/somePath"), QueryParam(("a", "1"),("b","2")):_*).asUriString shouldBe
      "http://someHost:80/somePath?a=1&b=2"
    Uri(Protocol("http"), HostName("someHost:80"), Path("/somePath"), QueryParam(("a +", "1"),("b","2 3")):_*).asUriString shouldBe
      "http://someHost:80/somePath?a+%2B=1&b=2+3"
  }
}
