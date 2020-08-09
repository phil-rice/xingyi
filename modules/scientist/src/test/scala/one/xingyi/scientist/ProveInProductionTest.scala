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
