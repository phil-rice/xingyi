/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.logging

import java.util.concurrent.atomic.AtomicReference

import one.xingyi.core.local.{LocalOps, LocalOpsPimper}
import one.xingyi.core.time.NanoTimeService

import scala.language.higherKinds

trait LoggingAdapter {
  def info(sender: Any)(msg: => String)

  def error(sender: Any)(msg: => String, t: Throwable)

  def debug(sender: Any)(msg: => String)

  def trace(sender: Any)(msg: => String)

}


trait LoggingAdapterWithDefaults extends LoggingAdapter {

  protected def log(sender: Any, level: String, msg: => String)

  protected def log(sender: Any, level: String, msg: => String, t: Throwable)

  override def info(sender: Any)(msg: => String): Unit = log(sender, "INFO", msg)

  override def error(sender: Any)(msg: => String, t: Throwable): Unit = log(sender, "ERROR", msg, t)

  override def debug(sender: Any)(msg: => String): Unit = log(sender, "DEBUG", msg)


  override def trace(sender: Any)(msg: => String): Unit = log(sender, "TRACE", msg)
}

class RememberLoggingAdapter(implicit nanoTimeService: NanoTimeService) extends LoggingAdapterWithDefaults {
  private val list = new AtomicReference(List[LoggingRecord]())
  def records: List[LoggingRecord] = list.get.reverse
  val startTime = nanoTimeService()
  override protected def log(sender: Any, level: String, msg: => String) = {
    val loggingRecord = LoggingRecord(nanoTimeService() - startTime, level, sender + "/" + msg, None)
    list.updateAndGet((list => loggingRecord :: list))
  }
  override protected def log(sender: Any, level: String, msg: => String, t: Throwable) = {
    val record = LoggingRecord(nanoTimeService() - startTime, level, sender + "/" + msg, Some(t))
    list.updateAndGet(list => record :: list)
  }
}

class RememberOrNormalAdapter[M[_]](delegate: LoggingAdapter)(implicit localOps: LocalOps[M]) extends LoggingAdapter with LocalOpsPimper[M] {
  def process(block: LoggingAdapter => Unit) = getFromLocalStore[LoggingAdapter] match {
    case Some(adapter) => block(adapter); block(delegate)
    case None => block(delegate)
  }
  override def info(sender: Any)(msg: => String) = process(_.info(sender)(msg))
  override def error(sender: Any)(msg: => String, t: Throwable) = process(_.error(sender)(msg, t))
  override def debug(sender: Any)(msg: => String) = process(_.debug(sender)(msg))
  override def trace(sender: Any)(msg: => String) = process(_.trace(sender)(msg))
}


object PrintlnLoggingAdapter extends LoggingAdapterWithDefaults {
  protected def log(sender: Any, level: String, msg: => String): Unit = println(s"[$level] $msg")

  protected def log(sender: Any, level: String, msg: => String, t: Throwable): Unit = {
    println(s"[$level]$msg   --  ${t.getClass.getSimpleName} ${t.getMessage}\n")
    t.printStackTrace(Console.out)
  }
}

object NullLoggingAdapter extends LoggingAdapterWithDefaults {
  override protected def log(sender: Any, level: String, msg: => String): Unit = {}

  override protected def log(sender: Any, level: String, msg: => String, t: Throwable): Unit = {}

}

