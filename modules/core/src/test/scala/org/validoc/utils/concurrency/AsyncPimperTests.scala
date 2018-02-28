package org.validoc.utils.concurrency

import java.util.concurrent.atomic.AtomicReference

import org.validoc.utils.{UtilsWithLoggingSpec, _}
import org.validoc.utils.functions.MonadWithException

import scala.concurrent.Future
import scala.language.higherKinds
import scala.util.{Failure, Success, Try}


abstract class AbstractAsyncPimperTests[M[_] : Async : MonadWithException] extends UtilsWithLoggingSpec {


  val runtimeException = new RuntimeException
  behavior of "Pimpers"

  it should "lift things" in {
    1.liftM[M].await() shouldBe 1
    //    Success(1).liftTry[M].await shouldBe 1
    val me1 = runtimeException.liftException[M, Int]
    intercept[RuntimeException](me1.await()) shouldBe runtimeException

    val me2 = Failure[Int](runtimeException).liftTry[M]
    intercept[RuntimeException](me2.await()) shouldBe runtimeException

  }
  it should "pimp a map" in {
    1.liftM[M].map(_ + 1).await() shouldBe 2
  }

  it should "pimp a flatMap" in {
    1.liftM[M].flatMap(x => (x + 1).liftM[M]).await() shouldBe 2
  }

  it should "pimp a transform" in {
    val fn = (tryI: Try[Int]) => tryI match {
      case Success(i) => (i + 1).liftM[M];
      case Failure(t) => 999.liftM[M]
    }

    1.liftM[M].mapTry(fn).await() shouldBe 2
    runtimeException.liftException[M, Int].mapTry(fn).await() shouldBe 999
  }

  it should "pimp a registersideeffect" in {
    val store = new AtomicReference[Try[Int]]()
    1.liftM[M].registerSideeffect(store.set).await()
    runtimeException.liftException[M, Int].registerSideeffect(store.set).await()
    store.get shouldBe Success(2)
  }
}

import org.validoc.utils.concurrency.AsyncForScalaFuture.ImplicitsForTest._
import org.validoc.utils.concurrency.AsyncForScalaFuture._

class FutureAsyncPimperTests extends AbstractAsyncPimperTests[Future]