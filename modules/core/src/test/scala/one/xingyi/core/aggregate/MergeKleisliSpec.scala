package one.xingyi.core.aggregate

import one.xingyi.core.{AsyncFixture, UtilsSpec}
import one.xingyi.core.functions.{Async, Monad, MonadCanFailWithException, ScalaFutureAsAsyncAndMonadAndFailer}

import scala.concurrent.Future
import scala.language.higherKinds
import scala.util.{Failure, Success}
import one.xingyi.core.language.Language._
import sun.nio.cs.ext.DoubleByteEncoder

abstract class MergeKleisliSpec[M[_], Fail](implicit monadCanFail: MonadCanFailWithException[M, Fail], async: Async[M]) extends UtilsSpec with AsyncFixture[M] {

  type Kleisli[Req, Res] = Req => M[Res]
  val merger = new MergeLanguage[Kleisli] with MergeKleisli[M] {
    override protected implicit def monad: Monad[M] = monadCanFail
  }

  import merger._

  behavior of "MergeKleisli"

  val runtimeException = new RuntimeException

  val k1: Int => M[String] = kleisli(1, Success("one"))
  val k2: Double => M[String] = kleisli(1.0, Success("two"))
  val k3: Int => M[String] = kleisli(1, Success("three"))
  val k4: Double => M[String] = kleisli(1, Success("four"))
  val kf: Int => M[String] = kleisli(1, Failure(runtimeException))

  implicit object FindReqForStringDouble extends FindReq[String, Double] {
    override def apply(v1: String): Double = Integer.parseInt(v1).toDouble
  }

  implicit object FindReqForStringInt extends FindReq[String, Int] {
    override def apply(v1: String): Int = Integer.parseInt(v1)
  }

  it should "merge 2 kleislis" in {
    val composite: String => M[String] = merge[Int, String](k1).and(k2).into[String, String]((req, i1, i2) => s"$req/$i1/$i2")
    composite("1").await() shouldBe "1/one/two"
  }
  it should "merge 3 kleislis" in {
    val composite: String => M[String] = merge[Int, String](k1) and k2 and k3 into[String, String] ((req, i1, i2, i3) => s"$req/$i1/$i2/$i3")
    composite("1").await() shouldBe "1/one/two/three"
  }
  it should "merge 4 kleislis" in {
    val composite: String => M[String] = merge[Int, String](k1) and k2 and k3 and k4 into[String, String] ((req, i1, i2, i3, i4) => s"$req/$i1/$i2/$i3/$i4")
    composite("1").await() shouldBe "1/one/two/three/four"
  }
}

import one.xingyi.core.functions.AsyncForScalaFuture._
import ImplicitsForTest._

class ScalaFutureMergeKleisliSpec extends MergeKleisliSpec[Future, Throwable] with ScalaFutureAsAsyncAndMonadAndFailer