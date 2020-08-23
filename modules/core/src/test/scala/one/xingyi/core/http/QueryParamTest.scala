/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.http

import one.xingyi.core.UtilsWithLoggingSpec


class QueryParamTest extends UtilsWithLoggingSpec {

  behavior of "QueryParam"

  it should "create a sequence of query params from a string like '?a=1&b=2'" in {
    QueryParam("") shouldBe Seq()
    QueryParam("?a=1") shouldBe Seq(QueryParam(QueryParamName("a"), QueryParamValue("1")))
    QueryParam("?a=1&b=2") shouldBe Seq(
      QueryParam(QueryParamName("a"), QueryParamValue("1")),
      QueryParam(QueryParamName("b"), QueryParamValue("2")))

  }

  it should "die if the params aren't a mutple of 2" in {
    intercept[QueryParamException](QueryParam("?a=1=justwrong&b=2"))
  }
}
