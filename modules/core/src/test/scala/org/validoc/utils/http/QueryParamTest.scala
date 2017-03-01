package org.validoc.utils.http

import org.validoc.utils.UtilsWithLoggingSpec


class QueryParamTest extends UtilsWithLoggingSpec {

  behavior of "QueryParam"

  it should "create a sequence of query params from a string like '?a=1&b=2'" in {
    QueryParam("") shouldBe Seq()
    QueryParam("?a=1") shouldBe Seq(QueryParam(QueryParamName("a"), QueryParamValue("1")))
    QueryParam("?a=1&b=2") shouldBe Seq(
      QueryParam(QueryParamName("a"), QueryParamValue("1")),
      QueryParam(QueryParamName("b"), QueryParamValue("2")))

  }

}
