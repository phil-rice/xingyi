package one.xingyi.core.http

import one.xingyi.core.UtilsSpec

class MethodSpec extends UtilsSpec {

  "Method()" should "return a method" in {
    Method("get") shouldBe Get
    Method("Get") shouldBe Get
    Method("Post") shouldBe Post
    Method("pUt") shouldBe Put
    Method("delete") shouldBe Delete
  }
}
