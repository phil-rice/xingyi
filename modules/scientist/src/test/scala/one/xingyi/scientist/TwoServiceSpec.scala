package one.xingyi.scientist

import one.xingyi.core.FunctionFixture
import one.xingyi.core.language.AnyLanguage._
import one.xingyi.core.monad.IdentityMonad
import one.xingyi.core.monad.IdentityMonad._
import org.scalatest.FlatSpecLike
import org.scalatestplus.mockito.MockitoSugar

import scala.language.higherKinds
import scala.util.Success


class TwoServiceSpec extends FlatSpecLike with  FunctionFixture with MockitoSugar   {

  behavior of "TwoService"

  it should "Call just process the left if only the left is selected" in {
    val twoServiceProcessor = new TwoServiceProcessor[IdentityMonad, Throwable, String, String] {
      override def selectAndTransformRequests: String => IdentityMonad[(Option[String], Option[String])] = k1("from", (Some("from1"), None))
      override def postProcessResults: TwoServiceMerger[IdentityMonad, String, String] = (raw, opt1, opt2) => {
        raw shouldBe "from"
        opt1 shouldBe Some("from1", "to1".liftM[IdentityMonad])
        opt2 shouldBe None
        "final".liftM[IdentityMonad]
      }
    }
    val service = new TwoService[IdentityMonad, Throwable, String, String](twoServiceProcessor, k1("from1", "to1"), k1("from2", "to2"))
    service("from").value shouldBe Success("final")
  }
  it should "Call just process the right if only the left is selected" in {
    val twoServiceProcessor = new TwoServiceProcessor[IdentityMonad, Throwable, String, String] {
      override def selectAndTransformRequests: String => IdentityMonad[(Option[String], Option[String])] = k1("from", (None, Some("from2")))
      override def postProcessResults: TwoServiceMerger[IdentityMonad, String, String] = (raw, opt1, opt2) => {
        raw shouldBe "from"
        opt1 shouldBe None
        opt2 shouldBe Some("from2", "to2".liftM[IdentityMonad])
        "final".liftM[IdentityMonad]
      }
    }
    val service = new TwoService[IdentityMonad, Throwable, String, String](twoServiceProcessor, k1("from1", "to1"), k1("from2", "to2"))
    service("from").value shouldBe Success("final")
  }
  it should "Call just process the left and right if both selected" in {
    val twoServiceProcessor = new TwoServiceProcessor[IdentityMonad, Throwable, String, String] {
      override def selectAndTransformRequests: String => IdentityMonad[(Option[String], Option[String])] = k1("from", (Some("from1"), Some("from2")))
      override def postProcessResults: TwoServiceMerger[IdentityMonad, String, String] = (raw, opt1, opt2) => {
        raw shouldBe "from"
        opt1 shouldBe Some("from1", "to1".liftM[IdentityMonad])
        opt2 shouldBe Some("from2", "to2".liftM[IdentityMonad])
        "final".liftM[IdentityMonad]
      }
    }
    val service = new TwoService[IdentityMonad, Throwable, String, String](twoServiceProcessor, k1("from1", "to1"), k1("from2", "to2"))
    service("from").value shouldBe Success("final")
  }

}
