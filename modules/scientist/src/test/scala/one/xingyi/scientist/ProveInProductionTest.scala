/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.scientist

import one.xingyi.core.language.AnyLanguage._
import one.xingyi.core.monad.IdentityMonad
import org.mockito
import org.mockito.Mockito._
import org.scalatest.{FlatSpec, Matchers}
import org.scalatestplus.mockito.MockitoSugar

import scala.util.{Failure, Success}

class ProveInProductionTest extends FlatSpec with Matchers with MockitoSugar {
  behavior of "ProveInProduction"

  implicit val cleanResultForDisplay: CleanResultForDisplay[String] = s => s + "_cleaned"

  it should "fail if nothing selected" in {
    implicit val resultComparator = mock[ResultComparator[String, String]]
    val prover = new ProveInProduction[IdentityMonad, Throwable, String, String]
    val Failure(e) = prover.postProcessResults("raw", None, None).value
    e.getMessage shouldBe "Nothing selected"
    verify(resultComparator, times(0)).compareAndReport(mockito.Matchers.any[String], mockito.Matchers.any[String], mockito.Matchers.any[String], mockito.Matchers.any[String], mockito.Matchers.any[String])
  }

  it should "return cleaned left if only left selected" in {
    implicit val resultComparator = mock[ResultComparator[String, String]]
    val prover = new ProveInProduction[IdentityMonad, Throwable, String, String]
    prover.postProcessResults("raw", Some("from1", "to1".liftM[IdentityMonad]), None).value shouldBe Success("to1_cleaned")
    verify(resultComparator, times(0)).compareAndReport(mockito.Matchers.any[String], mockito.Matchers.any[String], mockito.Matchers.any[String], mockito.Matchers.any[String], mockito.Matchers.any[String])

  }
  it should "return cleaned right if only right selected" in {
    implicit val resultComparator = mock[ResultComparator[String, String]]
    val prover = new ProveInProduction[IdentityMonad, Throwable, String, String]
    prover.postProcessResults("raw", None, Some("from2", "to2".liftM[IdentityMonad])).value shouldBe Success("to2_cleaned")
    verify(resultComparator, times(0)).compareAndReport(mockito.Matchers.any[String], mockito.Matchers.any[String], mockito.Matchers.any[String], mockito.Matchers.any[String], mockito.Matchers.any[String])

  }
  it should "return left, and send results to result comparator if both selected, and return the left after cleaning it" in {
    implicit val resultComparator = mock[ResultComparator[String, String]]
    val prover = new ProveInProduction[IdentityMonad, Throwable, String, String]
    prover.postProcessResults("raw", Some("from1", "to1".liftM[IdentityMonad]), Some("from2", "to2".liftM[IdentityMonad])).value shouldBe Success("to1_cleaned")
    verify(resultComparator, times(1)).compareAndReport("raw", "from1", "to1", "from2", "to2")
  }
}
