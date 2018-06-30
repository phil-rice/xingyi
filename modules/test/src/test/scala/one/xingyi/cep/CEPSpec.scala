package one.xingyi.cep
import one.xingyi.core.UtilsSpec
import one.xingyi.core.reflection.Macros

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.language.reflectiveCalls

trait CepFixture {
  val fraudtestbusinesseventstopic = Topic("fraudtestbusinesseventstopic", "1.0.0")
  val fraudtestinputtopic = Topic("fraudtestinputtopic", "1.0.0")

  trait CustomerAddressIpAddressAndType extends WithFields {
    val `type` = stringField
    val customerId = stringField
    val ipaddress = stringField
  }

  val pp2 = new Preprocess("pp2", "1.0.0") {
    val ie1 = new TopicEvent("ie1", fraudtestinputtopic) with CustomerAddressIpAddressAndType {
      where(`type`.value == "A")
    }
    val ie2 = new TopicEvent("ie2", fraudtestinputtopic) with CustomerAddressIpAddressAndType {
      where(`type`.value == "B" && customerId.value.matches("[A02468]$"))
    }
    val ie3 = new TopicEvent("ie3", fraudtestinputtopic) with CustomerAddressIpAddressAndType {
      where(`type`.value == "F" && customerId.value.matches("[A02468]$"))
    }

    val map123 = new MapEvent {
      val ipaddress = stringField :== ie1.ipaddress.value + ie2.ipaddress.value + ie3.ipaddress.value
      val `type` = stringField :== ie1.`type`.value + "-" + ie2.`type`.value + "-" + ie3.`type`.value
      val businessEventSubtype = stringField :== "performance-test-data"
    }

     val initial = state(ie1 >> ie1Recv)
     val ie1Recv = state(timeout(11 seconds) >> purge >> terminate || ie2 >> ie2Recv)
     val ie2Recv = state(ie3 >> map(map123) >> emit >> terminate)
  }

  val be2 = new TopicEvent("be2", fraudtestbusinesseventstopic, "1.0.0") with CustomerAddressIpAddressAndType

  case class bind(e: Preprocess) {
    def to(topic: TopicEvent) = ()
  }
    bind(pp2) to (be2)
}
class CEPSpec extends UtilsSpec with CepFixture with WithFields {

  behavior of "CEP"

  it should "do something while I'm just playing around" in {
    val ipaddress = stringField
    println(Macros.desugar(ipaddress :== "asd"))
    println

    val ie5 = new TopicEvent("ie5", fraudtestinputtopic) with CustomerAddressIpAddressAndType {
      println(Macros.desugar(where(`type`.value == "A")))
    }
    println
    println(pp2.initial)
    pp2.initialise
    println
    println(pp2.initial)
    println(pp2.ie1Recv)
    println(pp2.ie2Recv)
    println
    println(pp2)

  }
}
