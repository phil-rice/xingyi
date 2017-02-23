package org.validoc.utils.time

import java.util.concurrent.atomic.{AtomicLong, AtomicReference}

import org.scalatest.concurrent.Eventually
import org.scalatest.mock.MockitoSugar
import org.scalatest.{FlatSpec, Matchers}
import org.validoc.utils.concurrency.Async._
import org.validoc.utils.concurrency.MDCPropagatingExecutionContext

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}
class MockTimeService extends NanoTimeService {
  val i = new AtomicLong(1000)

  override def apply(): Long = i.addAndGet(100l)
}

class TimeServiceTest extends FlatSpec with Matchers with MockitoSugar with Eventually {

  implicit val ec: MDCPropagatingExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

  behavior of "TimeService"

  def setup(fn: (NanoTimeService, AtomicReference[(Try[String], Long)], (Try[String], Long) => Unit) => Unit) = {
    val timeService = new MockTimeService
    val ref = new AtomicReference[(Try[String], Long)]()
    fn(timeService, ref, (t, d) => ref.set((t, d)))
  }

  def await[T](f: Future[T]) = Await.result(f, 5 seconds)

  it should "time a Req => Future[Res] and report the time to a sideeffect" in {
    setup { (timeService, ref, sideeffect) =>
      await(timeService[Future, String, String] { x => (x + "_result").liftValue }(sideeffect).apply("x")) shouldBe "x_result"
      eventually(ref.get shouldBe ((Success("x_result"), 100l)))
    }
  }

  it should "time a Req => Future[Res] and report the time to a sideeffect when there is an exception" in {
    val runtimeException = new RuntimeException
    setup { (timeService, ref, sideeffect) =>
      intercept[RuntimeException](await(timeService[Future, String, String] { _ => runtimeException.liftThrowable }(sideeffect).apply("x"))) shouldBe runtimeException
      eventually(ref.get shouldBe ((Failure(runtimeException), 100l)))
    }
  }

}
