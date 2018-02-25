package org.validoc.utils.success

import java.text.MessageFormat
import java.util.ResourceBundle

import scala.util.{Failure, Success, Try}


case class MessageName[Req, Res](name: String) extends AnyVal

sealed trait SucceededState[Fail,T] {
  def asKey: String
  def fromBundle[Req, Res](implicit messageName: MessageName[Req, Res], bundle: ResourceBundle) = bundle.getString(messageName.name + "." + asKey)
  def format[Req, Res](strings: String*)(implicit messageName: MessageName[Req, Res], bundle: ResourceBundle) = MessageFormat.format(fromBundle[Req, Res], strings:_*)
}

case class SuccessState[Fail,T](t: T) extends SucceededState[Fail,T] {
  override def asKey: String = "success"
}

case class FailedState[Fail,T](fail: Fail) extends SucceededState[Fail,T] {
  override def asKey: String = "failure"
}

case class ExceptionState[Fail,T](t: Throwable) extends SucceededState[Fail,T] {
  override def asKey: String = "exception"
}