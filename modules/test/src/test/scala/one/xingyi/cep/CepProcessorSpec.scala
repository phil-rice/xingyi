package one.xingyi.cep

import one.xingyi.cep.model.{Event, KeyByStringField, StringField}
import one.xingyi.core.UtilsSpec
import org.scalatest.mockito.MockitoSugar

import scala.language.reflectiveCalls
import org.mockito.Mockito._
abstract class AbstractCepProcessorSpec[ED](implicit cep: CEP[ED]) extends UtilsSpec with CepFixture[ED] with MockitoSugar {

  def makeEd(tuples: (StringField, String)*): ED


  val falseKeyBy = KeyByStringField("notIn")
  val typeField = KeyByStringField("type")
  val customerIdField = KeyByStringField("customerId")
  val ipaddress = KeyByStringField("ipaddress")


  behavior of "CepProcessor.findLastStateFromED"
  it should "return nothing when the keyBy isn't in the ED" in {
    setup { cepProcessor =>
      cepProcessor.findLastStateFromED(makeEd()) shouldBe None
      cepProcessor.findLastStateFromED(makeEd(falseKeyBy -> "someValue")) shouldBe None
    }
  }

  it should "return a new miyamoto state when it's a new keyby, and return that the next time" in {
    setup { cepProcessor =>
      val ed = makeEd(pp2.keyby -> "someValue")
      val Some(state) = cepProcessor.findLastStateFromED(ed)
      state shouldBe StoredState("someValue", ed, pp2.initial, Map())
      cepProcessor.findLastStateFromED(ed).get should be theSameInstanceAs (state)
    }
  }
  behavior of "PipelineData.makeStartIfCan"


  it should "return none if it cannot accept the pipeline because no event at the start of the pipelines match" in {
    val oldEd = makeEd(pp2.keyby -> "someValue")
    val ed = makeEd(pp2.keyby -> "someValue", falseKeyBy -> "other")
    val state = StoredState("someValue", oldEd, pp2.test, Map())
    setup { cepProcessor =>
      PipelineData.makeIfCan(ed)(state) shouldBe None
    }

  }
  it should "replace the ed in the state, and find the first pipeline that would accept the ED in the current state " in {
    val oldEd = makeEd(pp2.keyby -> "someValue")
    setup { cepProcessor =>
      val ed = makeEd(pp2.keyby -> "someValue", typeField -> "A", ipaddress -> "someIpAddresss", falseKeyBy -> "shouldNotBeInclude")
      val start = StoredState("someValue", oldEd, pp2.test, Map())
      val expectedData = Map[Event, StringMap](pp2.ie1 -> Map("type" -> "A", "customerId" -> "someValue", "ipaddress" -> "someIpAddresss"))
      PipelineData.makeIfCan(ed)(start) shouldBe Some(PipelineData("someValue", ed, pp2.test, expectedData, pp2.test.list(0), pp2.ie1, List()))
    }
    setup { cepProcessor =>
      val ed = makeEd(pp2.keyby -> "someValue", typeField -> "B", ipaddress -> "someIpAddresss", falseKeyBy -> "shouldNotBeInclude")
      val start = StoredState("someValue", oldEd, pp2.test, Map())
      val expectedData = Map[Event, StringMap](pp2.ie2 -> Map("type" -> "B", "customerId" -> "someValue", "ipaddress" -> "someIpAddresss"))
      PipelineData.makeIfCan(ed)(start) shouldBe Some(PipelineData("someValue", ed, pp2.test, expectedData, pp2.test.list(1), pp2.ie2, List()))
    }
  }

  behavior of "execute"

  //  it should "pass the state to the execute method of the pipeline, and update the current state to be the final state in the pipeline" in {
  //    setup { cepProcessor =>
  //      val state = mock[StoredState[ED]]
  //      val finalMState = StoredState("someKey", makeEd(), terminate, Map())
  //      val pipeline = mock[StatePipeline]
  //      val tuple = StoreStateAndPipeline(pipeline, finalMState)
  //      when(pipeline.execute(finalMState)) thenReturn finalMState
  //      val finalState = mock[CepState]
  //      when(pipeline.finalState) thenReturn { () => finalState }
  //      cepProcessor.processPipeline(tuple) shouldBe finalMState.copy(currentState = finalState)
  //    }
  //  }

}

import one.xingyi.cep.CEPSpec._
class CepProcessorSpec extends AbstractCepProcessorSpec[Map[String, String]] {
  override def makeEd(tuples: (StringField, String)*): Map[String, String] = CEPSpec.makeEd(tuples: _*)

}
