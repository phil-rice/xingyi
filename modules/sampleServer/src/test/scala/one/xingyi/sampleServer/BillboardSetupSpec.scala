package one.xingyi.sampleServer

import one.xingyi.core.UtilsSpec
import one.xingyi.core.monad.AsyncForScalaFuture
import one.xingyi.json4s.Json4sWriter._
import one.xingyi.sample.BillboardSetup
import one.xingyi.core.monad.AsyncForScalaFuture._
import one.xingyi.sample.domain.{ProductionId, Promotion, PromotionQuery}

class BillboardSetupSpec extends UtilsSpec with AllProducersSetup {


  behavior of "BillboardSetup"
  lazy val setup = new BillboardSetup()

  it should "create a billboardsetup" in {
    setup.async.isInstanceOf[AsyncForScalaFuture] shouldBe true
    await(setup.billboard(PromotionQuery(false))) shouldBe Promotion(List(ProductionId("1",false), ProductionId("2",false), ProductionId("3",false)))
  }
}
