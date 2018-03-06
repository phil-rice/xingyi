package org.validoc.utils.logging

import java.text.MessageFormat
import java.util.ResourceBundle

import org.validoc.utils._
import org.validoc.utils.functions.MonadCanFailWithException
import org.validoc.utils.success.SuccessState

import scala.language.higherKinds
import scala.util.Try

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


trait LogRequestAndResult[Fail] {
  def apply[Req: DetailedLogging : SummaryLogging, Res: DetailedLogging : SummaryLogging](sender: Any, messagePrefix: String)(req: Req): Try[Either[Fail, Res]] => Unit
  protected def format(messagePrefix: String, messagePostFix: String)(strings: String*): String
  protected def details[T](t: T)(implicit detailedLogging: DetailedLogging[T]) = detailedLogging(t)
  protected def summary[T](t: T)(implicit summaryLogging: SummaryLogging[T]) = summaryLogging(t)
}

//TODO This is using inheritance! Clearly a mess... should be cleaned up
abstract class AbstractLogRequestAndResult[Fail: DetailedLogging : SummaryLogging](implicit log: LoggingAdapter) extends LogRequestAndResult[Fail] {
  override def apply[Req: DetailedLogging : SummaryLogging, Res: DetailedLogging : SummaryLogging](sender: Any, messagePrefix: String)(req: Req): Try[Either[Fail, Res]] => Unit = {
    def string[T: SummaryLogging : DetailedLogging](state: String, t: T) = format(messagePrefix, state)(summary(req), details(req), summary(t), details(t))
    SuccessState.sideffectWithString[Fail, Res](
      (state, t) => log.error(sender)(string(state, t), t),
      (state, fail) => log.info(sender)(string(state, fail)),
      (state, res) => log.debug(sender)(string(state, res)))
  }
}
class LogRequestAndResultForBundle[Fail: DetailedLogging : SummaryLogging](implicit bundle: ResourceBundle, log: LoggingAdapter) extends AbstractLogRequestAndResult[Fail] {
  override  def format(messagePrefix: String, messagePostFix: String)(strings: String*) =
    MessageFormat.format(bundle.getString(messagePrefix + "." + messagePostFix), strings: _*)
}

