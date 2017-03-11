package org.validoc.utils.concurrency

import java.util.concurrent.atomic.AtomicReference

import org.validoc.utils.UtilsWithLoggingSpec

import scala.concurrent.Future
import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

abstract class AbstractAsyncPimperTests[M[_]] extends UtilsWithLoggingSpec {

  import Async._

  implicit def async: Async[M]

  val runtimeException = new RuntimeException
  behavior of "Pimpers"

  it should "lift things" in {
    1.lift[M].await shouldBe 1
    1.liftValue[M].await shouldBe 1
    Success(1).liftTry[M].await shouldBe 1
    val me1 = runtimeException.liftThrowable[M, Int]
    intercept[RuntimeException](me1.await) shouldBe runtimeException

    val me2 = Failure[Int](runtimeException).liftTry[M]
    intercept[RuntimeException](me2.await) shouldBe runtimeException

  }
  it should "pimp a map" in {
    1.lift[M].map(_ + 1).await shouldBe 2
  }

  it should "pimp a flatMap" in {
    1.lift[M].flatMap(x => (x + 1).lift[M]).await shouldBe 2
  }

  it should "pimp a transform" in {
    val fn = (tryI: Try[Int]) => tryI match {
      case Success(i) => (i + 1).lift[M];
      case Failure(t) => 999.lift[M]
    }

    1.lift[M].transform(fn).await shouldBe 2
    runtimeException.liftThrowable[M, Int].transform(fn).await shouldBe 999
  }

  it should "pimp a registersideeffect" in {
    val store = new AtomicReference[Try[Int]]()
    1.lift[M].registerSideEffectWhenComplete(store.set).await
    store.get shouldBe Success(1)
  }
}

class FutureAsyncPimperTests extends AbstractAsyncPimperTests[Future] {


  override implicit def async: Async[Future] = Async.asyncForFuture
}