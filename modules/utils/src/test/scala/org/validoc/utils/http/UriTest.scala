package org.validoc.utils.http

import org.validoc.utils.UtilsSpec


class UriTest extends UtilsSpec {
  behavior of "Uri"

  it should "encode the data correctly  protocol" in {
    Uri(Protocol("http"), HostName("someHost"), Port(80), Path("/somePath")).asUriString shouldBe
      "http://someHost:80/somePath"
    Uri(Protocol("http"), HostName("someHost"), Port(80), Path("/somePath"), QueryParam(("a", "1")): _*).asUriString shouldBe
      "http://someHost:80/somePath?a=1"
    Uri(Protocol("http"), HostName("someHost"), Port(80), Path("/somePath"), QueryParam(("a", "1"), ("b", "2")): _*).asUriString shouldBe
      "http://someHost:80/somePath?a=1&b=2"
    Uri(Protocol("http"), HostName("someHost"), Port(80), Path("/somePath"), QueryParam(("a +", "1"), ("b", "2 3")): _*).asUriString shouldBe
      "http://someHost:80/somePath?a+%2B=1&b=2+3"
  }

  it should "be creatable from a string, default http and https" in {
    Uri("http://someHost") shouldBe
      Uri(Protocol("http"), HostName("someHost"), Port(80), Path(""))
    Uri("http://someHost/") shouldBe
      Uri(Protocol("http"), HostName("someHost"), Port(80), Path("/"))
    Uri("http://someHost/somePath") shouldBe
      Uri(Protocol("http"), HostName("someHost"), Port(80), Path("/somePath"))
    Uri("http://someHost/somePath?a=1&b=2") shouldBe
      Uri(Protocol("http"), HostName("someHost"), Port(80), Path("/somePath"),
        QueryParam(QueryParamName("a"), QueryParamValue("1")), QueryParam(QueryParamName("b"), QueryParamValue("2")))
    Uri("https://someHost/somePath?a=1&b=2") shouldBe
      Uri(Protocol("https"), HostName("someHost"), Port(443), Path("/somePath"),
        QueryParam(QueryParamName("a"), QueryParamValue("1")), QueryParam(QueryParamName("b"), QueryParamValue("2")))
    Uri("http://someHost:123/somePath?a=1&b=2") shouldBe
      Uri(Protocol("http"), HostName("someHost"), Port(123), Path("/somePath"),
        QueryParam(QueryParamName("a"), QueryParamValue("1")), QueryParam(QueryParamName("b"), QueryParamValue("2")))
  }


  it should "report error if unknown protocol given without a port" in {
    intercept[ProtocolException](Uri("ftp://someHost"))
  }

}
