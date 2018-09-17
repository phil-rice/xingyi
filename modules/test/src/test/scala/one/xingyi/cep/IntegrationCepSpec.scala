/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cep
import one.xingyi.cep.model.EmitData
import one.xingyi.core.UtilsSpec

import scala.language.reflectiveCalls

abstract class AbstractIntegrationCepSpec[ED: CEP] extends UtilsSpec with CepFixture[ED] {
  def makeEd(tuples: (String, String)*): ED

  behavior of "CepFixture"

  val edInitial = makeEd("customerId" -> "someValue", "type" -> "A", "ipaddress" -> "someIpAddress1", "junk" -> "someJunk")
  val edTwo = makeEd("customerId" -> "someValue", "type" -> "B", "ipaddress" -> "someIpAddress2", "junk" -> "someJunk")
  val edThree = makeEd("customerId" -> "someValue", "type" -> "C", "ipaddress" -> "someIpAddress3", "junk" -> "someJunk")

  it should "process the initial ED" in {
    setup { cepProcessor =>
      cepProcessor.process(edInitial)
      val Some(StoredState("someValue", actualEd, pp2.ie1Recv, actualMap)) = cepProcessor.findLastStateFromED(edInitial)
      actualEd shouldBe edInitial
      actualMap shouldBe Map(pp2.ie1 -> Map("customerId" -> "someValue", "type" -> "A", "ipaddress" -> "someIpAddress1"))
    }
  }

  it should "not process the initial ED a second time" in {
    setup { cepProcessor =>
      cepProcessor.process(edInitial)
      cepProcessor.process(edInitial) shouldBe None

      val Some(StoredState("someValue", actualEd, pp2.ie1Recv, actualMap)) = cepProcessor.findLastStateFromED(edInitial)
      actualEd shouldBe edInitial
      actualMap shouldBe Map(pp2.ie1 -> Map("customerId" -> "someValue", "type" -> "A", "ipaddress" -> "someIpAddress1"))
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
        pp2.ie1 -> Map("customerId" -> "someValue", "type" -> "A", "ipaddress" -> "someIpAddress1"),
        pp2.ie2 -> Map("customerId" -> "someValue", "type" -> "B", "ipaddress" -> "someIpAddress2")
      )
    }
  }
  it should "Process the third ED which has a map in it" in {
    setup { cepProcessor =>
      cepProcessor.process(edInitial)
      cepProcessor.process(edTwo)
      val Some(PipelineData("someValue", actualEd, actualState, actualMap, actualPipeline, actualLastEvent, actualEmit)) = cepProcessor.process(edThree)
      //      actualState shouldBe terminate
      actualEd shouldBe edThree
      actualState shouldBe pp2.ie2Recv
      val expectedMap = Map("ipaddress" -> "someIpAddress1/someIpAddress2/someIpAddress3", "type" -> "A-B-C", "businessEventSubtype" -> "performance-test-data")
      actualMap(pp2.map123) shouldBe expectedMap
      actualPipeline shouldBe pp2.ie2Recv.list(0)
      actualLastEvent shouldBe pp2.map123
      actualMap shouldBe Map(
        pp2.ie1 -> Map("customerId" -> "someValue", "type" -> "A", "ipaddress" -> "someIpAddress1"),
        pp2.ie2 -> Map("customerId" -> "someValue", "type" -> "B", "ipaddress" -> "someIpAddress2"),
        pp2.ie3 -> Map("customerId" -> "someValue", "type" -> "C", "ipaddress" -> "someIpAddress3"),
        pp2.map123 -> expectedMap
      )
      actualEmit shouldBe List(EmitData(expectedMap))
    }
  }

  it should "remove the data from the trip map when terminate occurs, so we get a new session" in {
    setup { cepProcessor =>
      cepProcessor.process(edInitial)
      cepProcessor.process(edTwo)
      cepProcessor.map.get("someValue").isDefined shouldBe true
      cepProcessor.process(edThree)
      cepProcessor.map.get("someValue").isDefined shouldBe false
    }
  }
}

import CEPSpec._
class IntegrationCepSpec extends AbstractIntegrationCepSpec[Map[String, String]] {
  override def makeEd(tuples: (String, String)*): Map[String, String] = tuples.toMap
}
