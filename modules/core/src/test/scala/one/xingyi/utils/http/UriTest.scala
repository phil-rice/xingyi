package one.xingyi.utils.http

import one.xingyi.utils.UtilsWithLoggingSpec


class UriTest extends UtilsWithLoggingSpec {
  behavior of "Uri"

  private val domainHttp = Domain(Protocol("http"), HostName("someHost"), Port(80))
  private val domainHttps = Domain(Protocol("https"), HostName("someHost"), Port(443))

  it should "encode the data correctly  protocol" in {
    Uri(Some(domainHttp), Path("/somePath")).asUriString shouldBe
      "http://someHost:80/somePath"
    Uri(Some(domainHttp), Path("/somePath"), QueryParam(("a", "1")): _*).asUriString shouldBe
      "http://someHost:80/somePath?a=1"
    Uri(Some(domainHttp), Path("/somePath"), QueryParam(("a", "1"), ("b", "2")): _*).asUriString shouldBe
      "http://someHost:80/somePath?a=1&b=2"
    Uri(Some(domainHttp), Path("/somePath"), QueryParam(("a +", "1"), ("b", "2 3")): _*).asUriString shouldBe
      "http://someHost:80/somePath?a+%2B=1&b=2+3"
  }

  it should "be creatable from a string, default http and https" in {
    Uri("http://someHost") shouldBe
      Uri(Some(domainHttp), Path(""))
    Uri("http://someHost/") shouldBe
      Uri(Some(domainHttp), Path("/"))
    Uri("http://someHost/somePath") shouldBe
      Uri(Some(domainHttp), Path("/somePath"))
    Uri("http://someHost/somePath?a=1&b=2") shouldBe
      Uri(Some(domainHttp), Path("/somePath"),
        QueryParam(QueryParamName("a"), QueryParamValue("1")), QueryParam(QueryParamName("b"), QueryParamValue("2")))
    Uri("https://someHost/somePath?a=1&b=2") shouldBe
      Uri(Some(domainHttps), Path("/somePath"),
        QueryParam(QueryParamName("a"), QueryParamValue("1")), QueryParam(QueryParamName("b"), QueryParamValue("2")))
    Uri("http://someHost:123/somePath?a=1&b=2") shouldBe
      Uri(Some(Domain(Protocol("http"), HostName("someHost"), Port(123))), Path("/somePath"),
        QueryParam(QueryParamName("a"), QueryParamValue("1")), QueryParam(QueryParamName("b"), QueryParamValue("2")))
  }

  it should "be creatable when domain not specified" in {
    Uri("/path") shouldBe Uri(None, Path("/path"))
    Uri("/path?param1=value1") shouldBe Uri(None, Path("/path"), QueryParam(QueryParamName("param1"), QueryParamValue("value1")))
    Uri("/path?param1=value1&param2=value2") shouldBe Uri(None, Path("/path"), QueryParam(QueryParamName("param1"), QueryParamValue("value1")), QueryParam(QueryParamName("param2"), QueryParamValue("value2")))
  }


  it should "report error if unknown protocol given without a port" in {
    intercept[ProtocolException](Uri("ftp://someHost"))
  }

  behavior of "Protocol"

  it should "enforce protocol is all letters" in {
    Protocol("some")
    intercept[IllegalArgumentException](Protocol("123"))
    intercept[IllegalArgumentException]( Protocol("som/one"))
  }
  it should "return the string as the asUriString" in {
    Protocol("some").asUriString shouldBe "some"
  }

  behavior of "port"

  it should "return the port as the asUriString" in {
    Port(123).asUriString shouldBe "123"
  }

}
