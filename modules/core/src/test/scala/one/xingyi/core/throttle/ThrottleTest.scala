package one.xingyi.core.throttle

import one.xingyi.core.UtilsSpec
import one.xingyi.core.throttle.Throttle.ThrottleStreamOps

import java.util.concurrent.ArrayBlockingQueue
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

class ThrottleTest extends UtilsSpec {

  behavior of "PutOnQueueThread"

  it should "put items on a queue as fast as it can" in {
    val capacity = 2
    val queue = new ArrayBlockingQueue[Int](capacity)
    val stream = Stream(1, 2, 3, 4, 5, 6)
    val thread = new PutOnQueueThread(stream, queue)
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

  behavior of "Throttle"

  implicit val throttleContext = ThrottleContext(2, Duration.Inf)

  it should "return a stream of the results - happy path" in {
    val stream = Stream(1, 2, 3, 4, 5, 6)
    stream.throttledMap((x: Int) => Future(x.toString)).take(6).toList shouldBe List("1", "2", "3", "4", "5", "6").map(Success(_))
  }

  it should "return a stream of the results when the passed in function returns a future exception" in {
    val stream = Stream(1, 2, 3, 4, 5, 6)
    val exception = new RuntimeException
    stream.throttledMap((x: Int) => Future.failed(exception)).take(6).toList shouldBe List.fill(6)(Failure[Int](exception))
  }
  it should "return a stream of the results when the passed in function returns an immediate exception" in {
    val stream = Stream(1, 2, 3, 4, 5, 6)
    val exception = new RuntimeException
    stream.throttledMap((x: Int) => throw exception).take(6).toList shouldBe List.fill(6)(Failure[Int](exception))
  }
}
