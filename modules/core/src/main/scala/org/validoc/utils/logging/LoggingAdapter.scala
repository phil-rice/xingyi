package org.validoc.utils.logging

import org.validoc.utils.strings.Strings


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

class RememberLoggingAdapter extends LoggingAdapterWithDefaults {
  private var list = List[LoggingRecord]()
  def records: List[LoggingRecord] = list.reverse
  override protected def log(sender: Any, level: String, msg: => String) = list = LoggingRecord(0, level, sender + "/" + msg, None) :: list
  override protected def log(sender: Any, level: String, msg: => String, t: Throwable) = list = LoggingRecord(0, level, sender + "/" + msg, Some(t)) :: list
}


object PrintlnLoggingAdapter extends LoggingAdapterWithDefaults {
  protected def log(sender: Any, level: String, msg: => String): Unit = println(s"[$level] $msg")

  protected def log(sender: Any, level: String, msg: => String, t: Throwable): Unit = {
    println(s"[level]$msg\n${t.getClass.getSimpleName} ${t.getMessage}\n")
    t.printStackTrace(System.out)
  }
}

object NullLoggingAdapter extends LoggingAdapterWithDefaults {
  override protected def log(sender: Any, level: String, msg: => String): Unit = {}

  override protected def log(sender: Any, level: String, msg: => String, t: Throwable): Unit = {}

}

