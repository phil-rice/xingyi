package one.xingyi.cep
import one.xingyi.cep.model.Event
import one.xingyi.core.UtilsSpec

import scala.language.reflectiveCalls
abstract class AbstractMapEventSpec[ED: CEP] extends UtilsSpec with CepFixture[ED] {

}

import one.xingyi.cep.CEPSpec._
class MapEventSpec extends AbstractMapEventSpec[StringMap] {
  behavior of "MapEvent"
  import pp2.map123
  it should "have fields" in {
    map123.fields.map(_.name) shouldBe List("ipaddress", "type", "businessEventSubtype")
  }
  it should "be able to calculate values for the fields" in {

    val map = Map[Event, StringMap](
      pp2.ie1 -> Map("customerId" -> "someValue", "type" -> "A", "ipaddress" -> "ip1"),
      pp2.ie2 -> Map("customerId" -> "someValue", "type" -> "B", "ipaddress" -> "ip2"),
      pp2.ie3 -> Map("customerId" -> "someValue", "type" -> "C", "ipaddress" -> "ip3")
    )
    val last = new LastEventAndDataForTest(map123, map)
    implicit val notUsedButNeedForCompileUntilGetMacrosSorted: LastEventAndData = mock[LastEventAndData]
    map123.findDataForThisEvent(last)
    map123.ipaddress.value shouldBe "ip1/ip2/ip3"
    map123.`type`.value shouldBe "A-B-C"
    map123.businessEventSubtype.value shouldBe "performance-test-data"
  }
}