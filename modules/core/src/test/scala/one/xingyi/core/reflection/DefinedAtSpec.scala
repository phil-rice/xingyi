package one.xingyi.core.reflection
import one.xingyi.core.UtilsSpec
import DefinedInSourceCodeAtLanguage._
class DefinedAtSpec extends UtilsSpec {

  behavior of "DefinedInSourceCodeAt"

  val s1 = DefinedInSourceCodeAt.definedInSourceCodeAt(1);
  val (s2a, s2b) = (DefinedInSourceCodeAt.definedInSourceCodeAt(1), DefinedInSourceCodeAt.definedInSourceCodeAt(1))
  it should "Allow the calculation of the line the call was made in" in {
    s1.st.toString shouldBe "one.xingyi.core.reflection.DefinedAtSpec.<init>(DefinedAtSpec.scala:8)"
  }

  implicit val isDefinedInSourceCodeAt: IsDefinedInSourceCodeAt[DefinedAtSpec] = _.s1

  it should "provide a pretty dump of the 'is defined at'" in {
    DefinedInSourceCodeAt(this).toString shouldBe "(DefinedAtSpec.scala:8)"
    this.definedInSourceCodeAt.toString shouldBe "(DefinedAtSpec.scala:8)"
  }

  it should "have an equals method" in {
    s1 shouldNot be(s2a)
    s2a should be(s2b)
    s1 shouldNot be("(DefinedAtSpec.scala:8)")
  }


  behavior of "CompositeDefinedInSourceCodeAt"

  it should "be created from the singles" in {
    val composite = CompositeDefinedInSourceCodeAt(Seq(s1, s2a, s2b))
    composite.toString shouldBe "Defined at[(DefinedAtSpec.scala:8),(DefinedAtSpec.scala:9),(DefinedAtSpec.scala:9)]"
  }
}
