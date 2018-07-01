package one.xingyi.cep
import one.xingyi.cep.model._
import one.xingyi.core.UtilsSpec
import one.xingyi.core.builder.{Aggregator, HasAggregator, RememberingAggregator2}

import scala.language.postfixOps
class StringFieldSpec extends UtilsSpec with EventWithFields {
  override def name: String = getClass.getSimpleName

  behavior of "StringField"

  it should "allow a variable to be created based on a string field" in {
    val s: StringField = stringField
    val test: StringField = stringField
    s.name shouldBe "s"
    test.name shouldBe "test"
  }
}
