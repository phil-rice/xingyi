package one.xingyi.sampleServer

import one.xingyi.core.UtilsSpec
import one.xingyi.core.monad.AsyncForScalaFuture
import one.xingyi.core.monad.AsyncForScalaFuture._
import one.xingyi.json4s.Json4sWriter._
import one.xingyi.sample.{BillboardSetup, VogueSetup}
import one.xingyi.sample.domain._

class VogueSetupSpec extends UtilsSpec with AllProducersSetup {


  behavior of "VogueSetup"
  lazy val setup = new VogueSetup()

  it should "create a billboardsetup" in {
    setup.async.isInstanceOf[AsyncForScalaFuture] shouldBe true
    await(setup.mostpopular(MostPopularQuery(false))) shouldBe MostPopular(List(ProgrammeId("1",false), ProgrammeId("2",false), ProgrammeId("3",false)))
  }
}
