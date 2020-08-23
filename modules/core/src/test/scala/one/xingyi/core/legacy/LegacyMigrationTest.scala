/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.legacy

import one.xingyi.core.language.Language.AnyOps
import one.xingyi.core.monad.{Async, MonadWithException, SuccessOrFail}
import one.xingyi.core.{FunctionFixture, UtilsSpec}

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.util.{Success, Try}
import one.xingyi.core.monad.IdentityMonad
import org.mockito.Mockito._
import one.xingyi.core.language.SuccessOrFailLanguage._
import one.xingyi.core.streams.StreamLanguage._

import scala.concurrent.Future

class AbstractLegacyMigrationTest[M[_] : MonadWithException, S[_] : SuccessOrFail](implicit async: Async[M]) extends UtilsSpec with FunctionFixture {

  behavior of getClass.getSimpleName


  def setup[InpAndOut, Inp, Out](toId: ForLegacy[M, InpAndOut, ID],
                                 toInp: ForLegacy[M, InpAndOut, Inp],
                                 toOut: ForLegacy[M, InpAndOut, Out],
                                 comparator: LegacyComparator[S, Out])
                                (block: (LegacyMigration[M, S, InpAndOut, Inp, Out]) => Unit): Unit = {
    block(LegacyMigration[M, S, InpAndOut, Inp, Out](toId, toInp, toOut, comparator))
  }

  val intToId: ForLegacy[M, String, ID] = i => ID(i.toString).liftM[M]
  val intToInp: ForLegacy[M, String, String] = s => (s + "_in").liftM[M]
  val intToOut: ForLegacy[M, String, String] = s => (s + "_out").liftM[M]


  val comparator: LegacyComparator[S, String] = {
    case (out, sOut) => sOut.fold(e => List(Difference(e.getClass.getSimpleName)), (repl: String) => List(Difference(out), Difference(repl)))
  }


  it should "have a map function that feeds the output of the original and the replacement to the comparator" in {
    setup(intToId, intToInp, intToOut, comparator) { (migration) =>
      async.await(migration.mapFn(k1[M, String, String]("x_in", "x_repl"))("x")) shouldBe
        FullLegacyResult[S, String, String, String]("x", ID("x"), "x_in", "x_out", DifferencesAndOut("x_repl".liftM[S], List(Difference("x_out"), Difference("x_repl"))))
    }
  }

  behavior of "FullLegacyReport when summarised and folded (a bit of an integration test) "

  it should " make a list of summary of differences" in {
    setup(intToId, intToInp, intToOut, comparator) { (migration) =>
      val mapFn = migration.mapFn(x => (x + "repl").liftM[M]) _
      Stream.from(1).map(_.toString).batchMapOverKleislis(3, mapFn).map(_.get).map(_.toSummary).take(3).addAll(ListOfSummaries.apply) shouldBe
        ListOfSummaries(List(
          SummaryOfDifference(ID("1"), List("1_out", "1_inrepl")),
          SummaryOfDifference(ID("2"), List("2_out", "2_inrepl")),
          SummaryOfDifference(ID("3"), List("3_out", "3_inrepl"))))
    }
  }
  it should " should ignore anything that has no difference" in {
    val comparator: LegacyComparator[S, String] = {
      case (out, _) if out == "2_out" => List()
      case (out, sOut) => sOut.fold(e => List(Difference(e.getClass.getSimpleName)), (repl: String) => List(Difference(out + "--" + repl)))
    }

    setup(intToId, intToInp, intToOut, comparator) { (migration) =>
      val mapFn = migration.mapFn(x => (x + "repl").liftM[M]) _
      Stream.from(1).map(_.toString).batchMapOverKleislis(3, mapFn).map(_.get).map(_.toSummary).take(3).addAll(ListOfSummaries.apply) shouldBe
        ListOfSummaries(List(
          SummaryOfDifference(ID("1"), List("1_out--1_inrepl")),
          SummaryOfDifference(ID("3"), List("3_out--3_inrepl"))))
    }
  }
}

class IdentityMonadLegacyMigrationTest extends AbstractLegacyMigrationTest[IdentityMonad, Try]
import one.xingyi.core.monad.AsyncForScalaFuture.ImplicitsForTest._
import one.xingyi.core.monad.AsyncForScalaFuture._


class ScalaFutureLegacyMigrationTest extends AbstractLegacyMigrationTest[Future, Try]
