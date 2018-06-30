package one.xingyi.cep

import one.xingyi.core.UtilsSpec

abstract class AbstractCepProcessorSpec[ED](implicit cep: CEP[ED]) extends UtilsSpec with CepFixture {

  def makeEd(tuples: (StringField, String)*): ED


  val falseKeyBy = KeyByStringField("notIn")
  val typeField = KeyByStringField("type")
  val customerIdField = KeyByStringField("customerId")
  val ipAddress = KeyByStringField("ipAddress")

  def setup(fn: CEPProcessor[ED] => Unit): Unit = {
    fn(new CEPProcessor[ED](fraudtestinputtopic, pp2))
  }
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
      state shouldBe MiyamotoState("someValue", ed, pp2.initial, Map())
      cepProcessor.findLastStateFromED(ed).get should be theSameInstanceAs (state)
    }
  }
  behavior of "CepProcessor.updateStateWithEd"

  it should "replace the ed in the state" in {
    setup { cepProcessor =>
      val ed = makeEd(pp2.keyby -> "someValue")
      val ed2 = makeEd(pp2.keyby -> "someValue", falseKeyBy -> "other")
      val start = MiyamotoState("someValue", ed, pp2.initial, Map())
      cepProcessor.updateStateWithEd(ed2)(start) shouldBe MiyamotoState("someValue", ed2, pp2.initial, Map())
    }
  }

  it should "return none if it cannot accept the pipeline" in {
    val ed = makeEd(pp2.keyby -> "someValue")
    val state = MiyamotoState("someValue", ed, pp2.test, Map())
    setup { cepProcessor =>
      cepProcessor.findPipeline(state) shouldBe None
    }

  }
  it should "find the first pipeline that would accept the ED in the current state " in {
    val ed = makeEd(pp2.keyby -> "someValue", typeField -> "A", ipAddress -> "someIpAddresss")
    val state = MiyamotoState("someValue", ed, pp2.test, Map())
    pp2.ie1.accepts(ed) shouldBe true

    setup { cepProcessor =>
      cepProcessor.findPipeline(state) shouldBe ""
    }

  }

}

import one.xingyi.cep.CEPSpec._
class CepProcessorSpec extends AbstractCepProcessorSpec[Map[String, String]] {
  override def makeEd(tuples: (StringField, String)*): Map[String, String] = CEPSpec.makeEd(tuples: _*)

}
