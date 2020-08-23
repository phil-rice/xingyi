/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.legacy

import java.util.Comparator

import one.xingyi.core.functions.Monoid
import one.xingyi.core.monad.{Async, MonadWithException, SuccessOrFail}
import one.xingyi.core.language.MonadLanguage._
import one.xingyi.core.language.SuccessOrFailLanguage._

import scala.language.higherKinds
import scala.util.Try

trait HasId[T] {
  def apply(t: T): ID
}
case class ID(id: String) extends AnyVal
case class Difference(diff: String) extends AnyVal
case class DifferencesAndOut[S[_], Out](out: S[Out], differences: List[Difference])
case class FullLegacyResult[S[_] : SuccessOrFail, InpAndOut, In, Out](inpAndOut: InpAndOut, id: ID, in: In, out: Out, result: DifferencesAndOut[S, Out]) {
  def toSummary: SummaryOfDifference = {
    val diff = result.out.fold(error => List(error.getClass.getCanonicalName), _ => Nil)
    SummaryOfDifference(id, diff ::: result.differences.map(_.diff))
  }
}
case class SummaryOfDifference(id: ID, result: List[String])

case class ListOfSummaries(list: List[SummaryOfDifference])

object ListOfSummaries {
  def apply(summaryOfDifference: SummaryOfDifference): ListOfSummaries = {
    if (summaryOfDifference.result.isEmpty) ListOfSummaries(List()) else ListOfSummaries(List(summaryOfDifference))
  }
  implicit object monoidForListOfSummaries extends Monoid[ListOfSummaries] {
    override def zero: ListOfSummaries = ListOfSummaries(List())
    override def add(t1: ListOfSummaries, t2: ListOfSummaries): ListOfSummaries =
      if (t1.list.isEmpty) t2 else if (t2.list.isEmpty) t1 else ListOfSummaries(t1.list ::: t2.list)
  }
}

trait ForLegacy[M[_], InpAndOut, Res] extends (InpAndOut => M[Res])
trait LegacyReporter[S[_], InpAndOut, Inp, Out] extends (Try[FullLegacyResult[S, InpAndOut, Inp, Out]] => Unit)
object LegacyReporter {
  def NullReporter[S[_], InpAndOut, Inp, Out]: LegacyReporter[S, InpAndOut, Inp, Out] = _ => {}
}
trait ThrowableToDifference extends (Throwable => List[Difference])
object ThrowableToDifference {
  implicit object defaultThrowableToDifference extends ThrowableToDifference {
    override def apply(v1: Throwable): List[Difference] = List(Difference(v1.getClass.getSimpleName))
  }
}
trait LegacyComparator[S[_], Out] extends ((Out, S[Out]) => List[Difference])

object LegacyComparator {
  def withTransforms[S[_] : SuccessOrFail, Out, CleanOut](oldTx: Out => CleanOut, newTx: Out => CleanOut, comparator: LegacyComparator[S, CleanOut]): LegacyComparator[S, Out] =
    (old, sRepl) =>
      comparator(oldTx(old), sRepl.map(newTx))

  val notEqualText = "not equal"
  def comparator[S[_] : SuccessOrFail, Out](implicit c: Comparator[Out], throwableToDifference: ThrowableToDifference): LegacyComparator[S, Out] =
    (old, sRepl) => sRepl.fold(throwableToDifference, repl => if (c.compare(old, repl) == 0) List() else List(Difference(notEqualText)))
  def equals[S[_] : SuccessOrFail, Out <: Comparable[Out]](implicit throwableToDifference: ThrowableToDifference): LegacyComparator[S, Out] =
    (old, sRepl) => sRepl.fold(throwableToDifference, repl => if (old == repl) List() else List(Difference(notEqualText)))
}
case class LegacyMigration[M[_] : MonadWithException, S[_] : SuccessOrFail, InpAndOut, Inp, Out]
(
  toId: ForLegacy[M, InpAndOut, ID],
  toInp: ForLegacy[M, InpAndOut, Inp],
  toOut: ForLegacy[M, InpAndOut, Out],
  comparator: LegacyComparator[S, Out],
) {
  def mapFn(replacementSystem: Inp => M[Out])(inpAndOut: InpAndOut): M[FullLegacyResult[S, InpAndOut, Inp, Out]] = {
    join3(
      toId(inpAndOut),
      toOut(inpAndOut),
      toInp(inpAndOut).flatMap(inp => replacementSystem(inp).toSuccessOrFail.map(out => (inp, out)))).map {
      case (id, out, (inp, sReplace)) =>
        //        println(s"id $id inp $inp out $out sReplace $sReplace")
        FullLegacyResult(inpAndOut, id, inp, out, DifferencesAndOut(sReplace, comparator(out, sReplace)))
    }
  }
}

