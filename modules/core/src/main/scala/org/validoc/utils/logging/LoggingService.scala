package org.validoc.utils.logging

import java.text.MessageFormat
import java.util.ResourceBundle

import org.validoc.utils._
import org.validoc.utils.functions.MonadCanFailWithException
import org.validoc.utils.success.SuccessState

import scala.language.higherKinds
import scala.util.{Failure, Success}

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

trait LogRequestAndResult[M[_], Fail] {
  def apply[Req: DetailedLogging : SummaryLogging, Res: DetailedLogging : SummaryLogging](sender: Any, messagePrefix: String)(req: Req): M[Res] => M[Res]
}


class LogRequestAndResultForBundle[M[_], Fail: DetailedLogging : SummaryLogging](implicit monad: MonadCanFailWithException[M, Fail], bundle: ResourceBundle, log: LoggingAdapter) extends LogRequestAndResult[M, Fail] {
  def details[T](t: T)(implicit detailedLogging: DetailedLogging[T]) = detailedLogging(t)
  def summary[T](t: T)(implicit summaryLogging: SummaryLogging[T]) = summaryLogging(t)
  def format(messagePrefix: String, messagePostFix: String)(strings: String*)(implicit bundle: ResourceBundle) =
    MessageFormat.format(bundle.getString(messagePrefix + "." + messagePrefix), strings: _*)
  //  def apply[Req: DetailedLogging : SummaryLogging, T: DetailedLogging : SummaryLogging](sender: Any, messagePrefix: String)(req: Req): M[Fail] => Unit = {
  //    case state@SuccessState(res) => log.debug(sender, state.format[Req, T](summary(req), summary(res)))
  //    case state@FailedState(fail) => log.debug(sender, state.format[Req, T](summary(req), details(req), summary(fail), details(fail)))
  //    case state@ExceptionState(t) => log.error(sender, state.format[Req, T](summary(req), details(req), t.getClass.getSimpleName, t.getMessage), t)
  //  }
  override def apply[Req: DetailedLogging : SummaryLogging, T: DetailedLogging : SummaryLogging](sender: Any, messagePrefix: String)(req: Req): M[T] => M[T] = { m =>
    m.onComplete {
      SuccessState.sideffectWithString[Fail, T](
        (state, t) => log.error(sender, format(messagePrefix, state)(summary(req), summary(t))),
        (state, fail) => log.info(sender, format(messagePrefix, state)(summary(req), summary(fail))),
        (state, res) => log.debug(sender, format(messagePrefix, state)(summary(req), summary(res))))
    }
  }
}

