package org.validoc.utils.logging

import org.validoc.utils.strings.Strings


trait LoggingAdapter {
  def info(sender: Any)(msg: => String)

  def error(sender: Any)(msg: => String, t: Throwable)

  def debug(sender: Any)(msg: => String)

  def trace(sender: Any)(msg: => String)

}

object LoggingAdapter {
  implicit lazy val adapter = {
    NullLoggingAdapterWithMdc
  }
}

trait LoggingAdapterWithMdc extends LoggingAdapter {
  private val map = new ThreadLocal[Map[String, String]] {
    override def initialValue(): Map[String, String] = Map()
  }

}

trait LoggingAdapterWithDefaults extends LoggingAdapter {

  protected def log(sender: Any, level: String, msg: => String)

  protected def log(sender: Any, level: String, msg: => String, t: Throwable)

  override def info(sender: Any)(msg: => String): Unit = log(sender, "INFO", msg)


  override def error(sender: Any)(msg: => String, t: Throwable): Unit = log(sender, "ERROR", msg, t)

  override def debug(sender: Any)(msg: => String): Unit = log(sender, "DEBUG", msg)


  override def trace(sender: Any)(msg: => String): Unit = log(sender, "TRACE", msg)
}


object PrintlnLoggingAdapter extends LoggingAdapterWithMdc with LoggingAdapterWithDefaults {
  protected def log(sender: Any, level: String, msg: => String): Unit = println(s"[$level] $msg")

  protected def log(sender: Any, level: String, msg: => String, t: Throwable): Unit = {
    println(s"[level]$msg\n${t.getClass.getSimpleName} ${t.getMessage}\n")
    t.printStackTrace(System.out)
  }
}

object NullLoggingAdapterWithMdc extends LoggingAdapterWithMdc with LoggingAdapterWithDefaults {
  override protected def log(sender: Any, level: String, msg: => String): Unit = {}

  override protected def log(sender: Any, level: String, msg: => String, t: Throwable): Unit = {}

}

