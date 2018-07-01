package one.xingyi.cep
import one.xingyi.core.UtilsSpec
import scala.language.reflectiveCalls

abstract class AbstractIntegrationCepSpec[ED: CEP] extends UtilsSpec with CepFixture[ED] {
  def makeEd(tuples: (String, String)*): ED

  behavior of "CepFixture"

  val edInitial = makeEd("customerId" -> "someValue", "type" -> "A", "ipaddress" -> "someIpAddress", "junk" -> "someJunk")
  val edTwo = makeEd("customerId" -> "someValue", "type" -> "B", "ipaddress" -> "someIpAddress", "junk" -> "someJunk")
  val edThree = makeEd("customerId" -> "someValue", "type" -> "C", "ipaddress" -> "someIpAddress", "junk" -> "someJunk")

  it should "process the initial ED" in {
    setup { cepProcessor =>
      cepProcessor.process(edInitial)
      val Some(StoredState("someValue", actualEd, pp2.ie1Recv, actualMap)) = cepProcessor.findLastStateFromED(edInitial)
      actualEd shouldBe edInitial
      actualMap shouldBe Map(pp2.ie1 -> Map("customerId" -> "someValue", "type" -> "A", "ipaddress" -> "someIpAddress"))
    }
  }

  it should "not process the initial ED a second time" in {
    setup { cepProcessor =>
      cepProcessor.process(edInitial)
      cepProcessor.process(edInitial) shouldBe None

      val Some(StoredState("someValue", actualEd, pp2.ie1Recv, actualMap)) = cepProcessor.findLastStateFromED(edInitial)
      actualEd shouldBe edInitial
      actualMap shouldBe Map(pp2.ie1 -> Map("customerId" -> "someValue", "type" -> "A", "ipaddress" -> "someIpAddress"))
    }
  }
  it should "not process the second ED initially" in {
    setup { cepProcessor =>
      cepProcessor.process(edTwo)
      val Some(StoredState("someValue", actualEd, pp2.initial, actualMap)) = cepProcessor.findLastStateFromED(edInitial)
      actualEd shouldBe edTwo
      actualMap shouldBe Map()
    }
  }

  it should "process the second ED after the first keeping the latest values" in {
    setup { cepProcessor =>
      cepProcessor.process(edInitial)
      cepProcessor.process(edTwo)
      val Some(StoredState("someValue", actualEd, pp2.ie2Recv, actualMap)) = cepProcessor.findLastStateFromED(edInitial)
      actualEd shouldBe edTwo
      actualMap shouldBe Map(
        pp2.ie1 -> Map("customerId" -> "someValue", "type" -> "A", "ipaddress" -> "someIpAddress"),
        pp2.ie2 -> Map("customerId" -> "someValue", "type" -> "B", "ipaddress" -> "someIpAddress")
      )
    }
  }
  it should "Process the third ED which has a map in it" in {
//    setup { cepProcessor =>
//      cepProcessor.process(edInitial)
//      cepProcessor.process(edTwo)
//      cepProcessor.process(edThree).isDefined shouldBe true
//      val Some(StoredState("someValue", actualEd, pp2.ie2Recv, actualMap)) = cepProcessor.findLastStateFromED(edInitial)
//      actualEd shouldBe edTwo
//      actualMap shouldBe Map(
//        pp2.ie1 -> Map("customerId" -> "someValue", "type" -> "A", "ipaddress" -> "someIpAddress"),
//        pp2.ie2 -> Map("customerId" -> "someValue", "type" -> "B", "ipaddress" -> "someIpAddress")
//      )
//    }

  }

}

import CEPSpec._
class IntegrationCepSpec extends AbstractIntegrationCepSpec[Map[String, String]] {
  override def makeEd(tuples: (String, String)*): Map[String, String] = tuples.toMap
}
