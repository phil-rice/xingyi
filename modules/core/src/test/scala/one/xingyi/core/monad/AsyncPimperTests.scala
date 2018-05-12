package one.xingyi.core.monad

import java.util.concurrent.atomic.AtomicReference

import one.xingyi.core.language.Language._
import one.xingyi.core.{UtilsWithLoggingSpec, _}

import scala.concurrent.Future
import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

abstract class AbstractAsyncPimperTests[M[_] : Async : MonadWithException] extends UtilsWithLoggingSpec with FunctionFixture {


  val runtimeException = new RuntimeException

  behavior of "FunctorPimper"

  it should "pimp a functor with .map" in {
    1.liftM[M].map(_ + 1).await() shouldBe 2
  }
  it should "pimp a functor with |=>" in {
    1.liftM[M].|=>(_ + 1).await() shouldBe 2
  }
  it should "pimp a functor with |+> that passes the parameter twice " in {
    1.liftM[M].|+>(x => y => x + y).await() shouldBe 2
  }


  behavior of "MonadPimper"

  it should "pimp a monad with .flatMap" in {
    1.liftM[M].flatMap(x => (x + 1).liftM[M]).await() shouldBe 2
  }

  it should "pimp a monad with |==> which does flatMap" in {
    1.liftM[M].flatMap(x => (x + 1).liftM[M]).await() shouldBe 2
  }

  it should "pimp a monad with m: M[A] |=*> (f: A => Seq[M[B]]) returning M[Seq[B]] " in {
    1.liftM[M] |=*> fn(1, Seq(1.liftM, 2.liftM)) await() shouldBe List(1, 2)
  }
  it should "pimp a monad with m: M[A] |=+> (f: A => A => M[B]) returning M[B] " in {
    1.liftM[M] |=+> fn2Curried(1, 1, "result".liftM[M]) await() shouldBe "result"
  }

  behavior of "MonadWithExceptionPimper"


  it should "add registerSideeffect to a monad" in {
    val atomicReference = new AtomicReference[Try[Int]]()
    1.liftM[M].registerSideeffect(sideeffect(atomicReference)).await() shouldBe 1
    atomicReference.get shouldBe Success(1)
    val m: M[Int] = runtimeException.liftException[M, Int].registerSideeffect(sideeffect(atomicReference))
    intercept[RuntimeException](m.await()) shouldBe runtimeException
    atomicReference.get shouldBe Failure(runtimeException)

  }


  it should "pimp a registersideeffect" in {
    val store = new AtomicReference[Try[Int]]()
    1.liftM[M].registerSideeffect(store.set).await()
    intercept[RuntimeException](runtimeException.liftException[M, Int].registerSideeffect(store.set).await()) shouldBe runtimeException
    store.get shouldBe Failure(runtimeException)
  }
}

import one.xingyi.core.monad.AsyncForScalaFuture.ImplicitsForTest._
import one.xingyi.core.monad.AsyncForScalaFuture._

class FutureAsyncPimperTests extends AbstractAsyncPimperTests[Future]