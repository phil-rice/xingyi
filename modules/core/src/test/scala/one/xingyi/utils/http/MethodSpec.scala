package one.xingyi.utils.http

import one.xingyi.utils.UtilsSpec

class MethodSpec extends UtilsSpec {

  "Method()" should "return a method" in {
    Method("get") shouldBe Get
    Method("Get") shouldBe Get
    Method("Post") shouldBe Post
    Method("pUt") shouldBe Put
    Method("delete") shouldBe Delete
  }
}
