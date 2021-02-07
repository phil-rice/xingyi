package one.xingyi.core.throttle

import one.xingyi.core.UtilsSpec

import java.util.concurrent.ArrayBlockingQueue
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}
import Throttle._
import one.xingyi.core.monad.AsyncForScalaFuture._
import one.xingyi.core.monad.AsyncForScalaFuture.ImplicitsForTest._

class ThrottleTest extends UtilsSpec {

  behavior of "PutOnQueueThread"

  it should "put items on a queue as fast as it can" in {
    val capacity = 2
    val queue = new ArrayBlockingQueue[Int](capacity)
    val list = Stream(1, 2, 3, 4, 5, 6)
    val thread = new PutOnQueueThread(list, queue)
    Thread.sleep(0) // not really needed. Just makes it so that we wait a tiny amount of time and give the queue thread a chance to mess up.
    queue.size() shouldBe 0
    thread.start()
    eventually(queue.size() shouldBe capacity)
    queue.take() shouldBe 1
    queue.take() shouldBe 2
    queue.take() shouldBe 3
    eventually(queue.size() shouldBe capacity)
    queue.take() shouldBe 4
    queue.take() shouldBe 5
    queue.take() shouldBe 6

    queue.size() shouldBe 0
  }

  behavior of "Throttle.throttledMap"

  implicit val throttleContext = ThrottleContext(2, Duration.Inf)

  it should "return a list of the results - happy path" in {
    val list = Stream(1, 2, 3, 4, 5, 6)
    list.throttledMap((x: Int) => Future(x.toString)).take(6).toList shouldBe List("1", "2", "3", "4", "5", "6").map(Success(_))
  }

  it should "return a list of the results when the passed in function returns a future exception" in {
    val list = Stream(1, 2, 3, 4, 5, 6)
    val exception = new RuntimeException
    list.throttledMap((x: Int) => Future.failed(exception)).take(6).toList shouldBe List.fill(6)(Failure[Int](exception))
  }
  it should "return a list of the results when the passed in function returns an immediate exception" in {
    val list = Stream(1, 2, 3, 4, 5, 6)
    val exception = new RuntimeException
    list.throttledMap((x: Int) => throw exception).take(6).toList shouldBe List.fill(6)(Failure[Int](exception))
  }

  behavior of "Throttle.throttle"

  it should "delegate to the  kleisli" in {
    val kleisli: Int => Future[String] = i => Future(i.toString)
    val throttled = kleisli.throttle(5)
    List(1, 2, 3, 4, 5).map(throttled).map(await) shouldBe List("1", "2", "3", "4", "5")
  }
}
