/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.http

import one.xingyi.core.UtilsWithLoggingSpec


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
