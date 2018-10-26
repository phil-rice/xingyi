package one.xingyi.sampleServer
import one.xingyi.core.UtilsSpec
import one.xingyi.core.UtilsSpec
import one.xingyi.core.monad.AsyncForScalaFuture
import one.xingyi.json4s.Json4sWriter._
import one.xingyi.sample.{BillboardSetup, FnordSetup, domain}
import one.xingyi.core.monad.AsyncForScalaFuture._
import one.xingyi.sample.domain.{Production, ProductionId, Promotion, PromotionQuery}
import one.xingyi.json4s.Json4sParser._
class FnordSetupSpec extends UtilsSpec with AllProducersSetup {

  behavior of "FnordSetup"
  lazy val setup = new FnordSetup()

  it should "be configured" in {
    setup.async.isInstanceOf[AsyncForScalaFuture] shouldBe true
    await(setup.production(domain.ProductionId("1",false))) shouldBe Production("from 1")
  }
}
