package org.validoc.utils.logging

import java.text.MessageFormat
import java.util.ResourceBundle

import org.validoc.utils._
import org.validoc.utils.functions.{MonadCanFailWithException, MonadWithException}
import org.validoc.utils.success._

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

case class RequestDetails[Req](req: Req, requestSummary: String)

trait DetailedLogging[T] extends (T => String)

object DetailedLogging {
  implicit def default[T] = new DetailedLogging[T] {
    override def apply(v1: T) = v1.toString
  }
}

trait SummaryLogging[T] extends (T => String)

object SummaryLogging {
  implicit def default[T] = new SummaryLogging[T] {
    override def apply(v1: T) = v1.toString
  }
}


class LogRequestAndResult[Fail: DetailedLogging : SummaryLogging](implicit bundle: ResourceBundle, log: LoggingAdapter) {
  def details[T](t: T)(implicit detailedLogging: DetailedLogging[T]) = detailedLogging(t)
  def summary[T](t: T)(implicit summaryLogging: SummaryLogging[T]) = summaryLogging(t)

  def apply[Req: DetailedLogging : SummaryLogging, Res: DetailedLogging : SummaryLogging](sender: Any)(req: Req)(implicit messageName: MessageName[Req, Res]): SucceededState[Fail, Res] => Unit = {
    case state@SuccessState(res) => log.debug(sender, state.format[Req, Res](summary(req), summary(res)))
    case state@FailedState(fail) => log.debug(sender, state.format[Req, Res](summary(req), details(req), summary(fail), details(fail)))
    case state@ExceptionState(t) => log.error(sender, state.format[Req, Res](summary(req), details(req), t.getClass.getSimpleName, t.getMessage), t)
  }
}

