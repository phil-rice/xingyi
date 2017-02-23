package org.validoc.utils.concurrency

import java.util
import java.util.concurrent._

import org.validoc.utils.logging.LoggingAdapter


trait DelegateExecutorService extends ScheduledExecutorService {

  protected def delegate: ScheduledExecutorService

  override def shutdown(): Unit = delegate.shutdown()

  override def isTerminated: Boolean = delegate.isTerminated

  override def awaitTermination(timeout: Long, unit: TimeUnit): Boolean = delegate.awaitTermination(timeout, unit)

  override def shutdownNow(): util.List[Runnable] = delegate.shutdownNow()

  override def isShutdown: Boolean = delegate.isShutdown


}

object MDCPropogatingExecutionScheduledExecutorService {

  import scala.language.implicitConversions

  implicit def wrap(e: ScheduledExecutorService) = new MDCPropogatingExecutionScheduledExecutorService(e)
}

class MDCPropogatingExecutionScheduledExecutorService(protected val delegate: ScheduledExecutorService)(implicit loggingAdapter: LoggingAdapter) extends DelegateExecutorService {
  protected def wrap(r: Runnable): Runnable = {
    val context = loggingAdapter.copyMDC
    new Runnable {
      def run(): Unit = {
        val oldContext = loggingAdapter.copyMDC
        try {
          loggingAdapter.setMDC(context)
          r.run()
        } finally {
          loggingAdapter.setMDC(oldContext)
        }
      }
    }
  }

  protected def wrap[T](c: Callable[T]): Callable[T] = {
    val context = loggingAdapter.copyMDC
    new Callable[T] {
      override def call(): T = {
        val oldContext = loggingAdapter.copyMDC
        try {
          loggingAdapter.setMDC(context)
          c.call()
        } finally {
          loggingAdapter.setMDC(oldContext)
        }
      }
    }
  }

  override def scheduleAtFixedRate(command: Runnable, initialDelay: Long, period: Long, unit: TimeUnit): ScheduledFuture[_] =
    delegate.scheduleAtFixedRate(wrap(command), initialDelay, period, unit)

  override def schedule(command: Runnable, delay: Long, unit: TimeUnit): ScheduledFuture[_] =
    delegate.schedule(wrap(command), delay, unit)

  override def schedule[V](callable: Callable[V], delay: Long, unit: TimeUnit): ScheduledFuture[V] =
    delegate.schedule(wrap(callable), delay, unit)

  override def scheduleWithFixedDelay(command: Runnable, initialDelay: Long, delay: Long, unit: TimeUnit): ScheduledFuture[_] =
    delegate.scheduleAtFixedRate(wrap(command), initialDelay, delay, unit)

  import collection.JavaConversions._

  override def invokeAll[T](tasks: util.Collection[_ <: Callable[T]]): util.List[Future[T]] =
    delegate.invokeAll(tasks.map { c: Callable[T] => wrap(c) })

  override def invokeAll[T](tasks: util.Collection[_ <: Callable[T]], timeout: Long, unit: TimeUnit): util.List[Future[T]] =
    delegate.invokeAll(tasks.map { c: Callable[T] => wrap(c) }, timeout, unit)

  override def invokeAny[T](tasks: util.Collection[_ <: Callable[T]]): T =
    delegate.invokeAny(tasks.map { c: Callable[T] => wrap(c) })

  override def invokeAny[T](tasks: util.Collection[_ <: Callable[T]], timeout: Long, unit: TimeUnit): T =
    delegate.invokeAny(tasks.map { c: Callable[T] => wrap(c) }, timeout, unit)

  override def submit[T](task: Callable[T]): Future[T] =
    delegate.submit(wrap(task))

  override def submit[T](task: Runnable, result: T): Future[T] =
    delegate.submit(wrap(task), result)

  override def submit(task: Runnable): Future[_] =
    delegate.submit(wrap(task))

  override def execute(command: Runnable): Unit =
    delegate.execute(wrap(command))

}
