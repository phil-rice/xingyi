package one.xingyi.core

import java.util.concurrent.atomic.AtomicReference

import one.xingyi.core.functions._
import one.xingyi.core.monad.AsyncForScalaFuture._
import ImplicitsForTest._
import org.scalatest.Matchers

import scala.concurrent.Future
import scala.language.higherKinds
import scala.util.{Failure, Success, Try}
import one.xingyi.core.language.Language._
import one.xingyi.core.monad.MonadCanFail

trait FunctionFixture extends Matchers{
  def fn[X, Y](expected: X, y: => Y) = { x: X => x shouldBe expected; y }
  def fn2[X, Y, Z](expectedX: X, expectedY: Y, z: => Z) = { (x: X, y: Y) => x shouldBe expectedX; y shouldBe expectedY; z }
  def fn2Curried[X, Y, Z](expectedX: X, expectedY: Y, z: => Z) = { x: X => y: Y => x shouldBe expectedX; y shouldBe expectedY; println(s"fn2 $expectedX, $expectedY, $z"); z }
  def sideeffect[X](atomicReference: AtomicReference[X]): X => Unit = atomicReference.set _

}

class PimperTests extends UtilsSpec with FunctionFixture {
  //  implicit class AnyPimper[T](t: T) {
  //    def |>[T2](fn: T => T2) = fn(t)
  //    def liftM[M[_]](implicit monad: Monad[M]): M[T] = monad.liftM(t)
  //    def |+>[T1](fn: T => T => T1): T1 = fn(t)(t)
  //    def liftException[M[_], T1](implicit async: MonadWithException[M], ev: T <:< Throwable): M[T1] = async.exception(t)
  //    def |?[M[_] : Functor, Failure](validation: T => Seq[Failure])(implicit withFailure: MonadCanFail[M, Failure], multipleFailures: FailureMaker[Seq[Failure], Failure]): M[T] =
  //      validation(t) match {
  //        case Nil => t.liftM
  //        case Seq(single) => withFailure.fail(single)
  //        case f => withFailure.fail(multipleFailures(f))
  //      }
  //  }

  val runtimeException = new RuntimeException("someErrorMessage")
  behavior of "AnyPimper"

  it should "have a |> that passes the any to a function" in {
    def fn[X](i: X) = i.toString
    1 |> fn shouldBe "1"
    1.0 |> fn shouldBe "1.0"
  }

  it should "have a liftM" in {
    1.liftM.await() shouldBe 1
    "1".liftM.await() shouldBe "1"
  }

  it should "have a |+> that passes the any to a function expecting it twice " in {
    def fn[X](i: X)(j: X) = s"$i + $j"
    1 |+> fn shouldBe "1 + 1"
    1.0 |+> fn shouldBe "1.0 + 1.0"
  }

  it should "have a liftException" in {
    val a = runtimeException.liftException[Future, Int]
    intercept[RuntimeException](a.await()) shouldBe runtimeException
  }

  def validate0[X](x: X) = Seq[String]()
  def validate1[X](x: X) = Seq("one")
  def validate2[X](x: X) = Seq("one,two")

  type M[T] = Either[String, T]
  val x: MonadCanFail[M, String] = MonadCanFail.monadCanFailForEither[String]
  def check[M[_], X](x: X, fn: X => Seq[String])(implicit m: MonadCanFail[M, String]): M[X] = x |?[M, String] fn

  it should "have |? that validates and returns the Right any when no validation issues" in {
    check(1, validate0)(x) shouldBe Right(1)
  }

  it should "have |? that validates and returns the left of monoid add all when validation issues" in {
    check(1, validate1)(x) shouldBe Left("one")
    check(1, validate2)(x) shouldBe Left("one,two")
  }

  behavior of "Boolean pimper"

  it should "have a to option that doesn't execute the value if false" in {
    false.toOption(throw new RuntimeException) shouldBe None
  }
  it should "have a to option that return Some(value) if true" in {
    true.toOption("x") shouldBe Some("x")
  }

  behavior of "Try Pimper"

  it should "lift a try into a monad" in {
    Success("1").liftTry[Future].await() shouldBe "1"
    val a = Failure(runtimeException).liftTry[Future]
    intercept[RuntimeException](a.await()) shouldBe runtimeException
  }

  behavior of "Function Pimper"


  it should "compose f1 ~> f2" in {
    (fn(1, "1") ~> fn("1", 2)) (1) shouldBe 2
  }

  it should "allow side effects to be called" in {
    val a = new AtomicReference[String]()
    (fn(1, "1") ~^> sideeffect(a)) (1) shouldBe "1"
  }

  it should "compose f ~+> g. f is A => B, g is A => B => C" in {
    (fn(1, "2") ~+> fn2Curried(1, "2", 3)) (1) shouldBe 3
  }

  it should "perform sideeffects using on f on EnterAndExit(mid , before, after)  f is A=>B, mid is A=>M, before is Mid => Unit, after is (Mid, B) => Unit   " in {
    val before = new AtomicReference[String]
    val after = new AtomicReference[(String, String)]
    (fn(1, "2") onEnterAndExit[String](fn(1, "mid"), sideeffect(before), (m, s) => sideeffect(after)(m, s))) (1) shouldBe "2"
    before.get shouldBe "mid"
    after.get shouldBe("mid", "2")
  }

  behavior of "OptionFunctionCurriedPimper"

  it should "chain of responsibility two option functions together. If the first returns a value that should be used" in {
    (fn2Curried(1, 2, Some(2)) chain fn2Curried[Int, Int, Some[Int]](1, 2, throw new RuntimeException)) (1)(2) shouldBe Some(2)
  }
  it should "chain of responsibility two option functions together. If the first returns None the second shoud be used" in {
    (fn2Curried(1, 2, None) chain fn2Curried[Int, Int, Option[Int]](1, 2, Some(3))) (1)(2) shouldBe Some(3)
    (fn2Curried(1, 2, None) chain fn2Curried[Int, Int, Option[Int]](1, 2, None)) (1)(2) shouldBe None
  }

  behavior of "Function2Pimper"

  it should "compose a fn2 followed by a fn" in {
    (fn2(1, 2, 3) ~> fn(3, 4)) (1, 2) shouldBe 4
  }

  behavior of "SeqFunctionPimper"

  it should "compose a fn A=>Seq[B] with B => C" in {
    (fn(1, Seq(2, 3, 4)) ~> (_ * 2)) (1) shouldBe List(4, 6, 8)
  }

  it should "compose f: A=>Seq[B] with g: B => M[C] using f ~~> g giving A => M[Seq[C]]" in {
    (fn(1, Seq(2, 3, 4)) ~~>(x => Future.successful(x * 2))).apply(1).await shouldBe List(4, 6, 8)
  }

  it should "compose f: A=>Seq[B] with g: B => M[C] using f ~+~> g giving A => M[Seq[(B, C)]]" in {
    (fn(1, Seq(2, 3, 4)) ~+>(x => Future.successful(x * 2))).apply(1).await shouldBe List((2,4), (3,6), (4,8))
  }


}
