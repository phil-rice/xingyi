package one.xingyi.cep
import java.io.ByteArrayOutputStream

import one.xingyi.cep.model._
import one.xingyi.core.UtilsSpec
import one.xingyi.core.builder.Aggregator

import scala.concurrent.duration._
import scala.language.{postfixOps, reflectiveCalls}


trait CepFixture[ED] {
  case class LastEventAndDataForTest(lastEvent: Event, data: Map[Event, StringMap]) extends LastEventAndData

  def setup(fn: CEPProcessor[ED] => Unit)(implicit cep: CEP[ED]): Unit = {
    fn(new CEPProcessor[ED](be2, pp2))
  }
  val fraudtestbusinesseventstopic = Topic("fraudtestbusinesseventstopic", "1.0.0")
  val fraudtestinputtopic = Topic("fraudtestinputtopic", "1.0.0")

  trait CustomerAddressIpAddressAndType extends EventWithFields {
    val `type` = stringField
    val customerId = stringField
    val ipaddress = stringField
  }

  val pp2 = new Processor("pp2", "1.0.0") {
    override val keyby = KeyByStringField("customerId")
    val ie1 = new TopicEvent("ie1", fraudtestinputtopic) with CustomerAddressIpAddressAndType {
      where(`type`.value == "A")
    }
    val ie2 = new TopicEvent("ie2", fraudtestinputtopic) with CustomerAddressIpAddressAndType {
      where(`type`.value == "B")
    }
    val ie3 = new TopicEvent("ie3", fraudtestinputtopic) with CustomerAddressIpAddressAndType {
      where(`type`.value == "C")
    }
    val map123 = new MapEvent("map123") {
      val ipaddress = stringField := ie1.ipaddress.value + "/" + ie2.ipaddress.value + "/" + ie3.ipaddress.value
      val `type` = stringField := ie1.`type`.value + "-" + ie2.`type`.value + "-" + ie3.`type`.value
      val businessEventSubtype = stringField := "performance-test-data"
    }

    val initial = state(ie1 >> ie1Recv)
    val ie1Recv = state(Timeout(11 seconds) >> purge >> Terminate || ie2 >> ie2Recv)
    val ie2Recv = state(ie3 >> map(map123) >> Emit >> Terminate)
    val test = state(ie1 >> ie1Recv || ie2 >> ie2Recv || ie3 >> Terminate)
  }
  pp2.initialise

  val be2 = new TopicEvent("be2", fraudtestbusinesseventstopic, "1.0.0") with CustomerAddressIpAddressAndType

  //  case class bind[ED:CEP](e: Preprocess) {
  //    def to(topic: TopicEvent) = new CEPProcessor[ED](topic, e)
  //  }

}
abstract class AbstractCEPSpec[ED](implicit stringGetter: StringFieldGetter[ED]) extends UtilsSpec with CepFixture[ED] with EventWithFields {
  override def name = getClass.getSimpleName

  def makeEd(tuples: (StringField, String)*): ED

  val ipaddress = stringField
  val `type` = stringField
  val customerId = stringField
  val otherField = stringField

  val stringFieldNotCreatedByMacro = new SimpleStringField(this, "someName")(new Aggregator[StringField] {
    override def apply(v1: StringField): Unit = {}
  })

  behavior of "stringfields"

  it should "should have their names equal to the variable name" in {
    ipaddress.name shouldBe "ipaddress"
    otherField.name shouldBe "otherField"
    stringFieldNotCreatedByMacro.name shouldBe "someName"
  }


  it should "have a value methods that pulls its value out of the 'currentfields' in 'withfields' (later this will be macro)" in {
    currentValues.set(new LastEventAndDataForAccept(this, Map(otherField.name -> "1", ipaddress.name -> "2", stringFieldNotCreatedByMacro.name -> "3")))
    implicitsValuesUntilWeGetMacrosSortedOut.dataForLastEvent(ipaddress.name) shouldBe "2"
    otherField.value shouldBe "1"
    ipaddress.value shouldBe "2"
    stringFieldNotCreatedByMacro.value shouldBe "3"
  }

  behavior of "withfields.update"

  it should "makes a filtered map based on the defined stringfields" in {
    val m = Map(otherField.name -> "2", ipaddress.name -> "3", stringFieldNotCreatedByMacro.name -> "1", customerId.name -> "4", `type`.name -> "5")
    findDataForThisEvent(new LastEventAndDataForAccept(this, m)) shouldBe Some(Map(otherField.name -> "2", ipaddress.name -> "3", customerId.name -> "4", `type`.name -> "5"))
  }

  it should "return none if there fields are not all there" in {
    Console.withOut(new ByteArrayOutputStream()) {
      Console.withErr(new ByteArrayOutputStream()) {
        findDataForThisEvent(new LastEventAndDataForAccept(NullEvent, Map(ipaddress.name -> "3", stringFieldNotCreatedByMacro.name -> "1"))) shouldBe None
        findDataForThisEvent(new LastEventAndDataForAccept(NullEvent, Map(otherField.name -> "3", stringFieldNotCreatedByMacro.name -> "1"))) shouldBe None
      }
    }
  }

  behavior of "TopicEvents"

  it should "be created in " in {
    pp2.ie1.name shouldBe "ie1"
    pp2.ie1.topic shouldBe fraudtestinputtopic
    pp2.ie1.fields.map(_.name) shouldBe List("type", "customerId", "ipaddress")
  }

  it should "only accept an event if the fields are present in the event" in {
    val edAll = makeEd(ipaddress -> "one", `type` -> "A", customerId -> "someId")
    val edMissing1 = makeEd(`type` -> "A", customerId -> "someId")
    val edMissing2 = makeEd(ipaddress -> "one", customerId -> "someId")
    val edMissing3 = makeEd(ipaddress -> "one", `type` -> "A")
    pp2.ie1.accepts(edAll) shouldBe true
    pp2.ie1.accepts(edMissing1) shouldBe false
    pp2.ie1.accepts(edMissing2) shouldBe false
    pp2.ie1.accepts(edMissing3) shouldBe false
  }

  it should "only accept an event if the where condition is true" in {
    val edAll = makeEd(ipaddress -> "one", `type` -> "A", customerId -> "someId")
    val edWithWrongType = makeEd(ipaddress -> "one", `type` -> "B", customerId -> "someId")
    pp2.ie1.accepts(edAll) shouldBe true
    pp2.ie1.accepts(edWithWrongType) shouldBe false
  }
}

object CEPSpec {
  implicit object CepForMapStringString extends CEP[Map[String, String]] {
    override def getString: StringFieldGetter[Map[String, String]] = stringField => ed => ed get stringField.name
    override def sendMessage(topicEvent: TopicEvent, emitData: EmitData): Unit = println(s"Sending to ${topicEvent.name} $emitData")
  }
  def makeEd(tuples: (StringField, String)*): Map[String, String] = tuples.map { case (k, v) => (k.name, v) }.toMap
}
import one.xingyi.cep.CEPSpec._
class CEPSpec extends AbstractCEPSpec[Map[String, String]] {

  override def makeEd(tuples: (StringField, String)*): Map[String, String] = CEPSpec.makeEd(tuples: _*)
}