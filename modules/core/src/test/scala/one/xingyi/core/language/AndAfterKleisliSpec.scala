package one.xingyi.core.language
import one.xingyi.core.{FunctionFixture, UtilsSpec}
import one.xingyi.core.monad.{Async, IdentityMonad, Monad}
import scala.language.higherKinds

abstract class AndAfterKleisliSpec[M[_] ](implicit val monad: Monad[M], async: Async[M]) extends UtilsSpec with AndAfterKleisli[M] with AnyLanguage with AsyncLanguage with  FunctionFixture {


  behavior of "AndAfterKleisli"

  it should "have an 'andAfter' that returns a new function " in {
    val f: Int => M[Int] = andAfter(fn(1, 2.liftM), fn(2, 3))
    f(1).await() shouldBe 3
  }
  it should "have an 'andAfterK' that returns a new function " in {
    val f: Int => M[Int] = andAfterK(fn(1, 2.liftM), fn(2, 3.liftM))
    f(1).await() shouldBe 3
  }
}

class IdentityAndAfterKleisliSpec extends AndAfterKleisliSpec[IdentityMonad]