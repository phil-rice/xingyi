package org.validoc.utils.concurrency

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import org.validoc.utils.UtilsSpec
import org.validoc.utils.concurrency.Async.AsyncPimper

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

abstract class AbstractAsyncTests[A[_]] extends UtilsSpec {

  implicit val async: Async[A]

  def awaitFor(a: A[String]): String

  def checkHasException[E <: Exception : ClassTag](a: A[String]): E

  behavior of getClass.getSimpleName

  it should "lift values into A" in {
    awaitFor(async.lift("someString")) shouldBe "someString"
  }

  it should "liftTry into A for successes" in {
    awaitFor(async.liftTry(Success("someString"))) shouldBe "someString"

  }
  it should "liftTry into A for failures" in {
    val runtimeException = new RuntimeException
    checkHasException[RuntimeException](async.liftTry(Failure(runtimeException))) shouldBe runtimeException
  }

  it should "execute map" in {
    awaitFor(async.map[String, String](async.lift("first"), x => x + "second")) shouldBe "firstsecond"
  }

  it should "execute flatMap" in {
    awaitFor(async.flatMap[String, String](async.lift("first"), x => async.lift(x + "second"))) shouldBe "firstsecond"
  }

  it should "async values into A on a different thread" in {
    val thisThread = Thread.currentThread().getName
    awaitFor(async.async(Thread.currentThread().getName)) shouldNot be(thisThread)
  }

  it should "execute sideeffects when done when A[String] created using lift and is successful" in {
    val count = new AtomicInteger
    val ref = new AtomicReference[Try[String]]()
    awaitFor(async.registerSideEffectWhenComplete(async.lift("someValue"), tryT => {
      ref.set(tryT);
      count.incrementAndGet()
    })) shouldBe "someValue"
    ref.get shouldBe Success("someValue")
    count.get shouldBe 1
  }

  it should "execute sideeffects when done when A[String] created using lift and has runtime exception" in {
    val count = new AtomicInteger
    val ref = new AtomicReference[Try[String]]()
    val runtimeException = new RuntimeException
    intercept[RuntimeException](awaitFor(async.registerSideEffectWhenComplete(async.lift(throw runtimeException), tryT => {
      ref.set(tryT)
      count.incrementAndGet()
    }))) shouldBe runtimeException
    ref.get shouldBe Failure(runtimeException)
    count.get shouldBe 1

  }

  it should "execute sideeffects when done when A[String] created using async and is successful" in {
    val count = new AtomicInteger
    val ref = new AtomicReference[Try[String]]()
    awaitFor(async.registerSideEffectWhenComplete(async.async("someValue"), tryT => {
      ref.set(tryT);
      count.incrementAndGet()
    })) shouldBe "someValue"
    ref.get shouldBe Success("someValue")
    count.get shouldBe 1
  }

  it should "execute sideeffects when done when A[String] created using async and has runtime exception" in {
    val count = new AtomicInteger
    val ref = new AtomicReference[Try[String]]()
    val runtimeException = new RuntimeException
    intercept[RuntimeException](awaitFor(async.registerSideEffectWhenComplete(async.async(throw runtimeException), tryT => {
      ref.set(tryT)
      count.incrementAndGet()
    }))) shouldBe runtimeException
    ref.get shouldBe Failure(runtimeException)
    count.get shouldBe 1
  }

  it should "transform successes" in {
    awaitFor(async.transform[String, String](async.lift("one"), _ match { case Success("one") => async.lift("two") })) shouldBe "two"
  }
  it should "transform failures" in {
    val runtimeException = new RuntimeException
    awaitFor(async.transform[String, String](async.liftTry(Failure(runtimeException)), _ match { case Failure(e) if e == runtimeException => async.lift("two") })) shouldBe "two"
  }
}

class FutureAsyncTests extends AbstractAsyncTests[Future] {
  override def awaitFor(a: Future[String]): String = Await.result(a, 5 seconds)

  override implicit val async: Async[Future] = Async.asyncForFuture

  override def checkHasException[E <: Exception : ClassTag](a: Future[String]) = {
    intercept[E](awaitFor(a))
  }
}
