package org.validoc.utils.concurrency

import scala.concurrent.duration.Duration

object DelayedFuture {

  import java.util.{Date, Timer, TimerTask}

  import scala.concurrent._
  import scala.concurrent.duration.FiniteDuration
  import scala.util.Try

  private val timer = new Timer(true)

  private def makeTask[T](body: => T)(schedule: TimerTask => Unit)(implicit ctx: ExecutionContext): Future[T] = {
    val prom = Promise[T]()
    schedule(
      new TimerTask {
        def run() {
          // IMPORTANT: The timer task just starts the execution on the passed
          // ExecutionContext and is thus almost instantaneous (making it
          // practical to use a single  Timer - hence a single background thread).
          ctx.execute(
            new Runnable {
              def run() {
                prom.complete(Try(body))
              }
            }
          )
        }
      }
    )
    prom.future
  }

  def apply[T](delay: Long)(body: => T)(implicit ctx: ExecutionContext): Future[T] = {
    makeTask(body)(timer.schedule(_, delay))
  }

  def apply[T](date: Date)(body: => T)(implicit ctx: ExecutionContext): Future[T] = {
    makeTask(body)(timer.schedule(_, date))
  }

  def apply[T](delay: FiniteDuration)(body: => T)(implicit ctx: ExecutionContext): Future[T] = {
    makeTask(body)(timer.schedule(_, delay.toMillis))
  }
  def apply[T](delay: Duration)(body: => Future[T])(implicit ctx: ExecutionContext): Future[T] = {
    makeTask()(timer.schedule(_, delay.toMillis)).flatMap(_ => body)
  }
}