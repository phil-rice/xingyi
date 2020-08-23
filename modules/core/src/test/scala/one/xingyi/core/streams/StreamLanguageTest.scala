package one.xingyi.core.streams

import one.xingyi.core.CoreSpec
import one.xingyi.core.language.Language.AnyOps
import one.xingyi.core.monad.{Async, IdentityMonad, MonadWithException, SuccessOrFail}
import one.xingyi.core.streams.StreamLanguage.StreamPimper

import scala.language.higherKinds
import scala.util.Try

abstract class StreamLanguageTest[M[_] : MonadWithException : Async, S[_]](implicit s: SuccessOrFail[S]) extends CoreSpec {

  behavior of "StreamLanguage"
  val e = new RuntimeException("Should not be visible")
  def mapFn(i: Int) = if (i % 3 == 0) throw e else i.toString.liftM[M]

  it should "batchMapOverKleislis a stream" in {
    Stream.from(1).batchMapOverKleislis[M, S, String](2, mapFn).take(6).toList shouldBe
      List("1".liftM[S], "2".liftM[S], s.exception(e), "4".liftM[S], "5".liftM[S], s.exception(e))
  }

  it should "map/reduce using the native streams map and foldLeft" in {
    Stream.from(1).batchMapOverKleislis[M, S, String](2,mapFn).take(10).addAll[String](v => s.fold(v, e => "e", (x: String) => x)) shouldBe "12e45e78e10"
  }

}
class IdentityMonadStreamLanguageTest extends StreamLanguageTest[IdentityMonad, Try]
