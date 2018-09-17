/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.logging

import java.text.MessageFormat
import java.util.ResourceBundle

import one.xingyi.core._
import one.xingyi.core.monad.MonadCanFailWithException
import one.xingyi.core.success.SuccessState

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
  def apply[Req: DetailedLogging : SummaryLogging, Res: DetailedLogging : SummaryLogging](sender: Any, messagePrefix: String): (Req, Try[Either[Fail, Res]]) => Unit
  protected def format(messagePrefix: String, messagePostFix: String)(strings: String*): String
  protected def details[T](t: T)(implicit detailedLogging: DetailedLogging[T]) = detailedLogging(t)
  protected def summary[T](t: T)(implicit summaryLogging: SummaryLogging[T]) = summaryLogging(t)
}

//TODO This is using inheritance! Clearly a mess... should be cleaned up
abstract class AbstractLogRequestAndResult[Fail: DetailedLogging : SummaryLogging](implicit log: LoggingAdapter) extends LogRequestAndResult[Fail] {
  override def apply[Req: DetailedLogging : SummaryLogging, Res: DetailedLogging : SummaryLogging](sender: Any, messagePrefix: String): (Req, Try[Either[Fail, Res]]) => Unit = { (req, t) =>
    def string[T: SummaryLogging : DetailedLogging](state: String, t: T) = format(messagePrefix, state)(summary(req), details(req), summary(t), details(t))
    SuccessState.sideffectWithString[Fail, Res](
      (state, t) => log.error(sender)(string(state, t), t),
      (state, fail) => log.info(sender)(string(state, fail)),
      (state, res) => log.debug(sender)(string(state, res)))(t)
  }
}
class LogRequestAndResultForBundle[Fail: DetailedLogging : SummaryLogging](implicit bundle: ResourceBundle, log: LoggingAdapter) extends AbstractLogRequestAndResult[Fail] {
  override def format(messagePrefix: String, messagePostFix: String)(strings: String*) =
    MessageFormat.format(bundle.getString(messagePrefix + "." + messagePostFix), strings: _*)
}

