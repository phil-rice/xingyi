package one.xingyi.utils

import one.xingyi.utils.functions.{MonadCanFail, ScalaFutureAsAsyncAndMonadAndFailer}
import one.xingyi.utils.language.Language._

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}
import org.mockito.Mockito._

class LanguageSpec extends UtilsSpec with ScalaFutureAsAsyncAndMonadAndFailer with AsyncFixture[Future] with FunctionFixture {

  val runtimeException = new RuntimeException
  behavior of "tryPimper"

  it should "allow a lift try that lifts a try into a liftable" in {
    Success(1).liftTry[Future].await() shouldBe 1
    intercept[RuntimeException](Failure(runtimeException).liftTry[Future].await()) shouldBe runtimeException
  }

  behavior of "AsyncLanguage"

  it should "have a liftTry method" in {
    liftTry(Success(1)).await() shouldBe 1
    intercept[RuntimeException](liftTry(Failure(runtimeException)).await) shouldBe runtimeException
  }

  behavior of "failure"

  it should "lift the failure into a MonadWithFailure" in {
    implicit val monad = mock[MonadCanFail[Option, String]]
    when(monad.fail[Int]("123")) thenReturn Some(42)
    "123".fail[Option, Int] shouldBe Some(42)
  }

  behavior of "MonadFunctionPimper"

  it should "have a kleisli composer that passes the output to a function|=>" in {
    (kleisli(1, Success(2)) |=> fn(2, 3)) (1).await() shouldBe 3
    intercept[RuntimeException]((kleisli(1, Failure(runtimeException)) |=> fn(2, 3)) (1).await() ) shouldBe runtimeException
    intercept[RuntimeException]((kleisli(1, Success(2)) |=> fn(2, throw runtimeException)) (1).await()) shouldBe runtimeException
  }

  it should "have a kleisli composer that passes the request as well as the result to a function Req => Res => Res2 using |=+>[" in {

    val k1: (Int => Future[Int]) = kleisli(1, Success(2))
    val k2: Int => Int => Int = fn2Curried(1, 2, 3)
    val composite: Int => Future[Int] = k1 |=+> k2
    composite(1).await() shouldBe 3
  }
  it should "have a kleisli composer that passes the request as well as the result to a function Req => Res => Res2 using |==+>[" in {
    val k1: (Int => Future[Int]) = kleisli(1, Success(2))
    val k2: Int => Int => Future[Int] = fn2Curried(1, 2, Future.successful(3))
    val composite: Int => Future[Int] = k1 |==+> k2
    composite(1).await() shouldBe 3
  }
  it should "have a kleisli arrow using |==>" in {
    val composite: (Int => Future[Int]) = (kleisli(1, Success(2)) |==> kleisli(2, Success(3)))
    composite(1).await() shouldBe 3
    intercept[RuntimeException]((kleisli(1, Failure(runtimeException)) |==> kleisli(2, Success(3))) (1).await) shouldBe runtimeException
    intercept[RuntimeException]((kleisli(1, Success(2)) |==> kleisli(2, Failure(runtimeException))) (1).await) shouldBe runtimeException
  }

}
