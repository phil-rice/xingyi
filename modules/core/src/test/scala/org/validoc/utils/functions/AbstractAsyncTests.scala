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
  def checkHasException[E <: Exception : ClassTag](a: A[String]) = intercept[E](awaitFor(a))

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

}

trait AbstractMonadTests[A[_]] extends AbstractFunctorTests[A] {
  def monad: Monad[A]
  override def functor: Monad[A] = monad

  behavior of s"Monad for ${monad.getClass.getSimpleName}"

  it should "lift values into A" in {
    awaitFor(monad.liftM("someString")) shouldBe "someString"
  }
}

abstract class AbstractMonadWithExceptionTests[A[_]](implicit m: MonadWithException[A]) extends AbstractMonadTests[A] {
  override def liftTryA[T](t: Try[T]): A[T] = m.liftTry(t)
  behavior of getClass.getSimpleName

  it should "liftTry into A for successes" in {
    awaitFor(m.liftTry(Success("someString"))) shouldBe "someString"
  }

  it should "liftTry into A for failures" in {
    val runtimeException = new RuntimeException
    checkHasException[RuntimeException](m.liftTry(Failure(runtimeException))) shouldBe runtimeException
  }

  it should "execute flatMap" in {
    awaitFor(m.flatMap[String, String](m.liftM("first"), x => m.liftM(x + "second"))) shouldBe "firstsecond"
  }


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
