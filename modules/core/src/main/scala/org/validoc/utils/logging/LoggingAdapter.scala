package org.validoc.utils.logging

import org.validoc.utils.strings.Strings
import org.validoc.utils.success.{ExceptionState, FailedState, SuccessState}

import scala.util.Try


sealed trait LogLevel {
  override def toString: String = Strings.classNameOfObject(this)
}

sealed trait LogLevelThatHasError extends LogLevel
object Info extends LogLevel
object Debug extends LogLevelThatHasError
object Error extends LogLevelThatHasError
object Trace extends LogLevel


trait LoggingAdapter{
  def info(sender: Any, msg: => String)

  def error(sender: Any, msg: => String)

  def error(sender: Any, msg: => String, t: Throwable)

  def debug(sender: Any, msg: => String)

  def debug(sender: Any, msg: => String, t: Throwable)

  def trace(sender: Any, msg: => String)

  def copyMDC: Map[String, String]

  def setMDC(mdc: Map[String, String])

  def setMDCvalue(name: String, value: String)

  def removeMDCvalue(name: String)

  def getMDCvalue(name: String): Option[String]

  def clearMdc = setMDC(Map())
}

object LoggingAdapter {
  implicit lazy val adapter = {NullLoggingAdapterWithMdc}
}

trait LoggingAdapterWithMdc extends LoggingAdapter {
  private val map = new ThreadLocal[Map[String, String]] {
    override def initialValue(): Map[String, String] = Map()
  }

  def copyMDC: Map[String, String] = map.get()

  def setMDC(mdc: Map[String, String]) = map.set(Map(mdc.toSeq: _*))

  def setMDCvalue(name: String, value: String) = map.set(map.get + (name -> value))

  def removeMDCvalue(name: String) = map.set(map.get - name)

  def getMDCvalue(name: String): Option[String] = map.get.get(name)
}

trait LoggingAdapterWithDefaults extends LoggingAdapter {

  protected def log(sender: Any, level: String, msg: => String)

  protected def log(sender: Any, level: String, msg: => String, t: Throwable)

  override def info(sender: Any, msg: => String): Unit = log(sender, "INFO", msg)

  override def error(sender: Any, msg: => String): Unit = log(sender, "ERROR", msg)

  override def error(sender: Any, msg: => String, t: Throwable): Unit = log(sender, "ERROR", msg, t)

  override def debug(sender: Any, msg: => String): Unit = log(sender, "DEBUG", msg)

  override def debug(sender: Any, msg: => String, t: Throwable): Unit = log(sender, "DEBUG", msg, t)

  override def trace(sender: Any, msg: => String): Unit = log(sender, "TRACE", msg)
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

  override def toString: String = s"NullLoggingAdapterWithMdc@${System.identityHashCode(this)}(trace=${copyMDC} "
}

