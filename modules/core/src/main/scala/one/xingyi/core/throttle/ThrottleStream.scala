package one.xingyi.core.throttle

import one.xingyi.core.language.MonadLanguage._
import one.xingyi.core.monad.{Async, MonadWithException}

import java.util.concurrent.{ArrayBlockingQueue, BlockingQueue}
import scala.concurrent.duration.Duration
import scala.util.Try
import scala.language.higherKinds

case class ThrottleContext(n: Int, duration: Duration)

object ThrottleContext {
  implicit def defaultThrottleContext: ThrottleContext = ThrottleContext(5, Duration.Inf)
}


object Throttle {
  def apply[M[_] : MonadWithException, From, To](n: Int, fn: From => M[To]): (From => M[To]) = {
    val array = new ArrayBlockingQueue[Int](n)
    from =>
      array.put(1); //doesnt matter what we put on it.
      fn(from).registerSideeffect(_ => array.take())
  }
  def mapAndThrottle[M[_] : MonadWithException, From, To](stream: Stream[From], fn: From => M[To])(implicit async: Async[M], throttleContext: ThrottleContext): Stream[Try[To]] = {
    val queue = new ArrayBlockingQueue[M[To]](throttleContext.n)
    new PutOnQueueThread(stream.map(wrapKleisliToCatchErrorsOnCall(fn)), queue).start()
    def result: Stream[Try[To]] = Try(async.await(queue.take(), throttleContext.duration)) #:: result
    result
  }
  def wrapKleisliToCatchErrorsOnCall[M[_], From, To](k: From => M[To])(implicit monad: MonadWithException[M]): (From => M[To]) =
    from => try {k(from) } catch {case e: Exception => monad.exception(e)}

  implicit class ThrottleStreamOps[T](s: Stream[T]) {
    def throttledMap[M[_] : Async : MonadWithException, To](fn: T => M[To])(implicit throttleContext: ThrottleContext): Stream[Try[To]] = mapAndThrottle(s, fn)
  }

  implicit class ThrottleKleisliOps[M[_] : MonadWithException, From, To](k: From => M[To]) {
    def throttle(n: Int): From => M[To] = Throttle(n, k)
  }

}

class PutOnQueueThread[T](stream: Stream[T], queue: BlockingQueue[T]) extends Thread {
  override def run(): Unit = stream.foreach(queue.put)
}