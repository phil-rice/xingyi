package one.xingyi.core.throttle

import java.util.concurrent.{ArrayBlockingQueue, BlockingQueue, TimeUnit}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Try

trait Throttle {
  def apply[From, To](fn: From => Future[To])(implicit throttleContext: ThrottleContext): (Stream[From] => Stream[Try[To]])
}

case class ThrottleContext(n: Int, duration: Duration)

object Throttle extends Throttle { //co-recursion
  implicit def defaultThrottle: Throttle = this
  implicit def defaultThrottleContext: ThrottleContext = ThrottleContext(5, Duration.Inf)

  implicit class ThrottleStreamOps[T](s: Stream[T]) {
    def throttledMap[To](fn: T => Future[To])(implicit throttleContext: ThrottleContext): Stream[Try[Any]] =
      Throttle(fn)(throttleContext)(s)
  }

  def wrapKleisliToCatchErrorsOnCall[From, To](k: From => Future[To]): (From => Future[To]) =
    from => try { k(from) } catch {case e: Exception => Future.failed(e)}

  def apply[From, To](fn: From => Future[To])(implicit throttleContext: ThrottleContext): Stream[From] => Stream[Try[To]] = {
    stream =>
      val queue = new ArrayBlockingQueue[Future[To]](throttleContext.n)
      new PutOnQueueThread(stream.map(wrapKleisliToCatchErrorsOnCall(fn)), queue).start()
      def result: Stream[Try[To]] = Try(Await.result(queue.take(), throttleContext.duration)) #:: result
      result
  }
}

class PutOnQueueThread[T](stream: Stream[T], queue: BlockingQueue[T]) extends Thread {
  override def run(): Unit = stream.foreach(queue.put)
}