package org.validoc.utils.functions

import java.util.concurrent.atomic.AtomicReference

import org.validoc.utils.{UtilsWithLoggingSpec, _}

import scala.concurrent.Future
import scala.language.higherKinds
import scala.util.{Failure, Success, Try}
import org.validoc.utils.language.Language._

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

  implicit class MonadWithExceptionPimper[M[_], T](m: M[T])(implicit monad: MonadWithException[M]) {
    def foldException[T1](fnE: Throwable => M[T1], fn: T => M[T1]): M[T1] = monad.foldException(m, fnE, fn)
    def mapTry[T1](fn: Try[T] => M[T1]): M[T1] = monad.foldException(m, t => fn(Failure(t)), { t: T => fn(Success(t)) })
    def registerSideeffect(fn: Try[T] => Unit): M[T] = monad.foldException(m, { e: Throwable => fn(Failure(e)); monad.exception(e) }, { t: T => fn(Success(t)); monad.liftM(t) })
  }

  it should "add foldException to a monad executing happy path when have value" in {
    (1.liftM[M] foldException[Int](_ => new RuntimeException().liftException, fn(1, 2.liftM)) await()) shouldBe 2
  }


  it should "add foldException to a monad executing exception path when have exception" in {
    val m: M[Int] = runtimeException.liftException[M, Int] foldException(x => throw new RuntimeException(x), _ => fail())
    intercept[RuntimeException](m.await()).getCause shouldBe runtimeException
  }

  it should "add mapTry to a monad" in {
    1.liftM[M].mapTry { case Success(1) => 2.liftM; case _ => 3.liftM }.await() shouldBe 2
    runtimeException.liftException[M, Int].mapTry { case Success(1) => 2.liftM; case _ => 3.liftM }.await shouldBe 3
  }
  it should "add registerSideeffect to a monad" in {
    val atomicReference = new AtomicReference[Try[Int]]()
    1.liftM[M].registerSideeffect(sideeffect(atomicReference)).await() shouldBe 1
    atomicReference.get shouldBe Success(1)
    val m: M[Int] = runtimeException.liftException[M, Int].registerSideeffect(sideeffect(atomicReference))
    intercept[RuntimeException](m.await()) shouldBe runtimeException
    atomicReference.get shouldBe Failure(runtimeException)

  }


  //  behavior of "Pimpers"


  it should "pimp a transform" in {
    val fn: Try[Int] => M[Int] = {
      _ match {
        case Success(i) => (i + 1).liftM
        case Failure(t) => 999.liftM
      }
    }
    1.liftM[M].mapTry(fn).await() shouldBe 2
    runtimeException.liftException[M, Int].mapTry(fn).await() shouldBe 999
  }

  it should "pimp a registersideeffect" in {
    val store = new AtomicReference[Try[Int]]()
    1.liftM[M].registerSideeffect(store.set).await()
    intercept[RuntimeException](runtimeException.liftException[M, Int].registerSideeffect(store.set).await()) shouldBe runtimeException
    store.get shouldBe Failure(runtimeException)
  }
}

import org.validoc.utils.functions.AsyncForScalaFuture.ImplicitsForTest._
import org.validoc.utils.functions.AsyncForScalaFuture._

class FutureAsyncPimperTests extends AbstractAsyncPimperTests[Future]