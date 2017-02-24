package org.validoc.utils.concurrency

import org.validoc.utils.logging.LoggingAdapter

import scala.concurrent.ExecutionContext


class MDCPropagatingExecutionContext(wrapped: ExecutionContext)(implicit loggingAdapter: LoggingAdapter) extends ExecutionContext {
  self =>
  override def prepare(): ExecutionContext = new ExecutionContext {
    // Save the call-site MDC state
    val context = loggingAdapter.copyMDC

    println(s"In MDCPropagatingExecutionContext.prepare. MDC is $context")
    protected def wrap(r: Runnable) = new Runnable {
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

    def execute(r: Runnable): Unit = self.execute(wrap(r))

    def reportFailure(t: Throwable): Unit = self.reportFailure(t)
  }

  override def execute(r: Runnable): Unit = wrapped.execute(r)

  override def reportFailure(t: Throwable): Unit = wrapped.reportFailure(t)
}


object MDCPropagatingExecutionContext {

  import scala.language.implicitConversions

  implicit def wrap(wrapped: ExecutionContext)(implicit loggingAdapter: LoggingAdapter): MDCPropagatingExecutionContext =
    new MDCPropagatingExecutionContext(wrapped)

}