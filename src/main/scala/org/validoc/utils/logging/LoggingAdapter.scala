package org.validoc.utils.logging


trait Logging {
  protected def info(msg: String) = LoggingAdapter.info(this, msg)

  protected def error(msg: String) = LoggingAdapter.error(this, msg)

  protected def error(msg: String, t: Throwable) = LoggingAdapter.error(this, msg, t)

  protected def debug(msg: String) = LoggingAdapter.debug(this, msg)

  protected def debug(msg: String, t: Throwable) = LoggingAdapter.debug(this, msg, t)

  protected def trace(msg: String) = LoggingAdapter.trace(this, msg)
}


trait LoggingAdapter {
  protected def info(sender: Any, msg: String)

  protected def error(sender: Any, msg: String)

  protected def error(sender: Any, msg: String, t: Throwable)

  protected def debug(sender: Any, msg: String)

  protected def debug(sender: Any, msg: String, t: Throwable)

  protected def trace(sender: Any, msg: String)
}

object LoggingAdapter extends LoggingAdapter {
  private var adapter: LoggingAdapter = PrintlnLoggingAdapter

  def info(sender: Any, msg: String) = adapter.info(sender, msg)

  def error(sender: Any, msg: String) = adapter.error(sender, msg)

  def error(sender: Any, msg: String, t: Throwable) = adapter.error(sender, msg, t)

  def debug(sender: Any, msg: String) = adapter.debug(sender, msg)

  def debug(sender: Any, msg: String, t: Throwable) = adapter.debug(sender, msg, t)

  def trace(sender: Any, msg: String) = adapter.trace(sender, msg)
}


object PrintlnLoggingAdapter extends LoggingAdapter {
  private def log(sender: Any, level: String, msg: String): Unit = println(s"[$level] $msg")

  private def log(sender: Any, level: String, msg: String, t: Throwable): Unit = {
    println(s"[level]$msg\n${t.getClass.getSimpleName} ${t.getMessage}\n")
    t.printStackTrace(System.out)
  }

  override def info(sender: Any, msg: String): Unit = log(sender, "INFO", msg)

  override def error(sender: Any, msg: String): Unit = log(sender, "ERROR", msg)

  override def error(sender: Any, msg: String, t: Throwable): Unit = log(sender, "ERROR", msg, t)

  override def debug(sender: Any, msg: String): Unit = log(sender, "DEBUG", msg)

  override def debug(sender: Any, msg: String, t: Throwable): Unit = log(sender, "DEBUG", msg, t)

  override def trace(sender: Any, msg: String): Unit = log(sender, "TRACE", msg)
}


