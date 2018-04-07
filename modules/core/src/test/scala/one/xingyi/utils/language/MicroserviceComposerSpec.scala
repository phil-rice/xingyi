package one.xingyi.utils.language

import one.xingyi.utils.functions.ScalaFutureAsAsyncAndMonadAndFailer
import one.xingyi.utils.{AsyncFixture, UtilsSpec}

import scala.concurrent.Future
import scala.util.Success
import Language._
import one.xingyi.utils.endpoint.EndPoint

class MicroserviceComposerSpec extends UtilsSpec with AsyncFixture[Future] with ScalaFutureAsAsyncAndMonadAndFailer with MicroserviceComposers[Future] {

  behavior of "MicroserviceComposer"

  it should "use |+| compose a kleisli with a transformer" in {
    val k1 = kleisli[Int, String](1, Success("two"))
    val composite = k1 |+| kleisliTransformer[Int, String, String, Int](k1, kleisli("five", Success(6)))
    composite("five").await shouldBe 6
  }
  it should "use |++| to turn a kleisli into an endpoint" in {
    val k1 = kleisli[Int, String](1, Success("two"))
    val endpoint = mock[EndPoint[Future, Int, String]]
    val actual = k1 |++| { k: (Int => Future[String]) => endpoint }
    actual shouldBe endpoint
  }

}
