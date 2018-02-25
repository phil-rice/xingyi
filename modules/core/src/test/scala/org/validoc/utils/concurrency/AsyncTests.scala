package org.validoc.utils.concurrency

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import org.scalatest.BeforeAndAfter
import org.validoc.utils.UtilsWithLoggingSpec
import org.validoc.utils.functions.MonadWithException

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.{higherKinds, postfixOps}
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

abstract class AbstractAsyncTests[A[_]](implicit val  async: Async[A], m: MonadWithException[A]) extends UtilsWithLoggingSpec {


  def awaitFor[X](a: A[X]): X = async.await(a)

  def checkHasException[E <: Exception : ClassTag](a: A[String]): E

  behavior of getClass.getSimpleName

  it should "lift values into A" in {
    awaitFor(m.liftM("someString")) shouldBe "someString"
  }

  it should "liftTry into A for successes" in {
    awaitFor(m.liftTry(Success("someString"))) shouldBe "someString"

  }
  it should "liftTry into A for failures" in {
    val runtimeException = new RuntimeException
    checkHasException[RuntimeException](m.liftTry(Failure(runtimeException))) shouldBe runtimeException
  }

  it should "execute map" in {
    awaitFor(m.map[String, String](m.liftM("first"), x => x + "second")) shouldBe "firstsecond"
  }

  it should "execute flatMap" in {
    awaitFor(m.flatMap[String, String](m.liftM("first"), x => m.liftM(x + "second"))) shouldBe "firstsecond"
  }

  it should "async values into A on a different thread" in {
    val thisThread = Thread.currentThread().getName
    awaitFor(async.async(Thread.currentThread().getName)) shouldNot be(thisThread)
  }

}

import scala.concurrent.ExecutionContext.Implicits._
import AsyncForScalaFuture._

class FutureAsyncTests extends AbstractAsyncTests[Future] with BeforeAndAfter {

  before(loggingAdapter.clearMdc)


  override def checkHasException[E <: Exception : ClassTag](a: Future[String]) = {
    intercept[E](awaitFor(a))
  }


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
