/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.concurrency

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

//  def apply[T](delay: Long)(body: => T)(implicit ctx: ExecutionContext): Future[T] = {
//    makeTask(body)(timer.schedule(_, delay))
//  }
//
//  def apply[T](date: Date)(body: => T)(implicit ctx: ExecutionContext): Future[T] = {
//    makeTask(body)(timer.schedule(_, date))
//  }
//
//  def apply[T](delay: FiniteDuration)(body: => T)(implicit ctx: ExecutionContext): Future[T] = {
//    makeTask(body)(timer.schedule(_, delay.toMillis))
//  }
  def apply[T](delay: Duration)(body: => Future[T])(implicit ctx: ExecutionContext): Future[T] = {
    makeTask()(timer.schedule(_, delay.toMillis)).flatMap(_ => body)
  }
}
