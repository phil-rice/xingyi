package one.xingyi.core.reflection

import one.xingyi.core.UtilsSpec

import scala.reflect.ClassTag

class ClassTagsSpec extends UtilsSpec {


  "ClassTags.clazz" should "return the class of the parameter" in {
    ClassTags.clazz[String] shouldBe classOf[String]
  }
  "ClassTags.nameOf" should "return the name of the parameter" in {
    ClassTags.nameOf[String] shouldBe "String"

  }
  "ClassTags.isA" should "return true if the parameter 'isa'" in {
    ClassTags.isA[String]("") shouldBe true
    ClassTags.isA[Int]("") shouldBe false

  }
  "ClassTags.collectAll" should "return all of the items in the collection that 'isa'" in {
    ClassTags.collectAll[Number](Seq(1, 1.0, "1")) shouldBe Seq(1, 1.0)
  }

  import ClassTags._

  "AnyPimperForClassTags.is" should "return true if the parameter 'isa'" in {
    "".is[String] shouldBe true
    1.is[String] shouldBe false
  }
  "IterablePimperForClassTags.collectAs" should "return all of the items in the collection that 'isa'" in {
    (Seq(1, 1.0, "1")).collectAs[Number] shouldBe Seq(1, 1.0)

  }

}
