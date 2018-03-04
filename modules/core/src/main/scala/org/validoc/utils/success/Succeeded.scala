package org.validoc.utils.success

import java.text.MessageFormat
import java.util.ResourceBundle

import scala.util.{Failure, Success, Try}


sealed trait SucceededState[Fail, T] {
  def asKey: String
  def fromBundle[Req, Res](messagePrefix: String)(implicit bundle: ResourceBundle) = bundle.getString(messagePrefix + "." + asKey)
  def format[Req, Res](messagePrefix: String, strings: String*)(implicit bundle: ResourceBundle) = MessageFormat.format(fromBundle[Req, Res](messagePrefix), strings: _*)
}

case class SuccessState[Fail, T](t: T) extends SucceededState[Fail, T] {
  override def asKey: String = "success"
}

case class FailedState[Fail, T](fail: Fail) extends SucceededState[Fail, T] {
  override def asKey: String = "failure"
}

case class ExceptionState[Fail, T](t: Throwable) extends SucceededState[Fail, T] {
  override def asKey: String = "exception"
}