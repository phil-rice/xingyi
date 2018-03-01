package org.validoc.utils.functions

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import org.scalatest.BeforeAndAfter
import org.validoc.utils.UtilsWithLoggingSpec
import org.validoc.utils.reflection.ClassTags

import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.language.{higherKinds, postfixOps}
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

trait ContainerSpec[A[_]] extends UtilsWithLoggingSpec {

  def liftTryA[T](t: Try[T]): A[T]
  def liftA[T](t: T): A[T] = liftTryA(Success(t))
  def awaitFor[X](a: A[X]): X
  def checkHasException[E <: Exception : ClassTag](a: A[String]): E = intercept[E](awaitFor(a))

}

trait AbstractAsyncTests[A[_]] extends UtilsWithLoggingSpec {
  def async: Async[A]

  def awaitFor[X](a: A[X]): X = async.await(a)

  behavior of s"Async for ${async.getClass.getSimpleName}"

  val runtimeException = new RuntimeException
  it should "lift values into A which can be awaited for" in {
    awaitFor(async.async("someString")) shouldBe "someString"
  }
  it should "lift exceptions into A which can be awaited for" in {
    val a: A[String] = async.async(throw runtimeException)
    intercept[RuntimeException](awaitFor(a)) shouldBe runtimeException
  }

  it should "have a respond method that is called after the monad gains a value" in {
    val count = new AtomicInteger()
    val remembered = new AtomicReference[Try[String]]()
    awaitFor(async.respond[String](async.async("someString"), { tryR =>
      remembered.set(tryR)
      count.incrementAndGet()
    })) shouldBe "someString"
    count.get shouldBe 1
    remembered shouldBe Success("someString")
  }
  it should "have a respond method that is called after the monad throws an exception" in {
    val count = new AtomicInteger()
    val remembered = new AtomicReference[Try[String]]()
    val a: A[String] = async.async[String](throw runtimeException)
    intercept[RuntimeException](awaitFor(async.respond[String](a, { tryR =>
      remembered.set(tryR)
      count.incrementAndGet()
    }))) shouldBe runtimeException
    count.get shouldBe 1
    remembered shouldBe Failure(runtimeException)
  }


  it should "async values into A on a different thread" in {
    val thisThread = Thread.currentThread().getName
    awaitFor(async.async(Thread.currentThread().getName)) shouldNot be(thisThread)
  }
}

trait AbstractFunctorTests[A[_]] extends ContainerSpec[A] {
  implicit def functor: Functor[A]

  behavior of s"Functor for ${functor.getClass.getSimpleName}"

  it should "execute map" in {
    awaitFor(functor.map[String, String](liftA("first"), x => x + "second")) shouldBe "firstsecond"
  }

  it should "execute map when an exception occurs in the functor" in {
    val runtimeException = new RuntimeException
    val a = functor.map(liftTryA(Failure[String](runtimeException)), { x: String => x + 1 })
    checkHasException[RuntimeException](a) shouldBe runtimeException
  }
  it should "execute map when an exception occurs in the fn" in {
    val runtimeException = new RuntimeException
    val a = functor.map[String, String](liftTryA(Success("1")), { x: String => throw runtimeException })
    checkHasException[RuntimeException](a) shouldBe runtimeException
  }
}

trait AbstractMonadTests[A[_]] extends AbstractFunctorTests[A] {
  def monad: Monad[A]
  override def functor: Monad[A] = monad

  behavior of s"Monad for ${monad.getClass.getSimpleName}"

  it should "lift values into A" in {
    awaitFor(monad.liftM("someString")) shouldBe "someString"
  }

  it should "execute flatMap" in {
    awaitFor(monad.flatMap[String, String](monad.liftM("first"), x => monad.liftM(x + "second"))) shouldBe "firstsecond"
  }

  it should "execute flatMap when a exception in monad" in {
    val runtimeException = new RuntimeException
    val a = functor.flatMap[String, String](liftTryA(Failure[String](runtimeException)), { x: String => monad.liftM("a") })
    checkHasException[RuntimeException](a) shouldBe runtimeException
  }

  it should "execute flatMap when an exception occurs in the fn" in {
    val runtimeException = new RuntimeException
    val a = functor.flatMap[String, String](liftTryA(Success("1")), { x: String => throw runtimeException })
    checkHasException[RuntimeException](a) shouldBe runtimeException
  }

  it should "execute join2" in {
    awaitFor(monad.join2(liftA(1), liftA(2))) shouldBe(1, 2)
  }

  it should "execute join3" in {
    awaitFor(monad.join3(liftA(1), liftA(2), liftA(3))) shouldBe(1, 2, 3)
  }

  it should "execute join4" in {
    awaitFor(monad.join4(liftA(1), liftA(2), liftA(3), liftA(4))) shouldBe(1, 2, 3, 4)
  }

}

abstract class AbstractMonadWithExceptionTests[A[_]](implicit m: MonadWithException[A]) extends AbstractMonadTests[A] {
  override def liftTryA[T](t: Try[T]): A[T] = m.liftTry(t)
  val runtimeException = new RuntimeException("someString")
  behavior of s"monad with exception ${getClass.getSimpleName}"

  it should "be able to lift an exception" in {
    checkHasException[RuntimeException](m.exception(runtimeException)) shouldBe runtimeException
  }

  it should "liftTry into A for successes" in {
    awaitFor(m.liftTry(Success("someString"))) shouldBe "someString"
  }

  it should "liftTry into A for failures" in {
    checkHasException[RuntimeException](m.liftTry(Failure(runtimeException))) shouldBe runtimeException
  }

  it should "recover from exceptions" in {
    awaitFor(m.recover(liftTryA(Failure(runtimeException)), _.getMessage)) shouldBe "someMessage"
  }

  it should "fold when the value is present" in {
    awaitFor(m.foldException[String, String](liftTryA(Success("value")), _.getMessage, _ + "happened")) shouldBe "value_happened"

  }

  it should "fold when the an exception is present" in {
    awaitFor(m.foldException[String, String](liftTryA(Failure(runtimeException)), _.getMessage, Functions.identify)) shouldBe "someMessage"

  }
}

abstract class AbstractMonadCanFailTests[A[_], Fail: ClassTag](implicit m: MonadCanFail[A, Fail]) extends AbstractMonadWithExceptionTests[A] {
  behavior of s"monad can fail ${getClass.getSimpleName} with fail being ${ClassTags.nameOf[Fail]}"

  def makeFail(s: String): Fail
  def failToString(f: Fail): String
  def callFold(a: A[String]) = m.foldWithFail[String, String](a, e => m.liftM(e.getMessage), f => m.liftM(failToString(f)), x => m.liftM(x + "happened"))

  it should "work with fold value" in {
    callFold(liftA("A")) shouldBe "Ahappened"
  }

  it should "work with fold exception" in {
    callFold(liftTryA(Failure(runtimeException))) shouldBe "someString"
  }
  it should "work with fold failure" in {
    callFold(m.fail(makeFail("fail"))) shouldBe "fail"
  }

  //  def fail[T](f: Fail): M[T]
  //  def foldWithFail[T, T1](m: M[T], fnE: Exception => M[T1], fnFailure: Fail => M[T1], fn: T => M[T1]): M[T1]
  //  def onComplete[T](m: M[T], fn: Try[Either[Fail, T]] => Unit): M[T] = foldWithFail[T, T](m,
  //
  //  def mapTryFail[T, T1](m: M[T], fn: Try[Either[Fail, T]] => M[T1]): M[T1] = foldWithFail[T, T1](m,


}


import org.validoc.utils.functions.AsyncForScalaFuture.ImplicitsForTest._
import org.validoc.utils.functions.AsyncForScalaFuture._

class FutureMonadWithExceptionTests(implicit val async: Async[Future], val monad: Monad[Future]) extends AbstractMonadWithExceptionTests[Future] with AbstractAsyncTests[Future] with BeforeAndAfter {

  before(loggingAdapter.clearMdc)

  it should "propogate the MDC with async, restoring MDC at end" in {
    loggingAdapter.setMDCvalue("someKey", "someValue")
    await(async.async {
      loggingAdapter.getMDCvalue("someKey") shouldBe Some("someValue")
      loggingAdapter.clearMdc
      "someResult"
    }) shouldBe "someResult"
    loggingAdapter.getMDCvalue("someKey") shouldBe Some("someValue")
  }

}
