package org.validoc.utils

import java.util.concurrent.atomic.AtomicReference

import org.validoc.utils.functions._
import AsyncForScalaFuture._
import ImplicitsForTest._

import scala.concurrent.Future
import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

class PimperTests extends UtilsSpec {
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

  //  implicit class FunctionPimper[Req, Res](fn: Req => Res) {
  //    def ~>[Res2](fn2: Res => Res2): (Req) => Res2 = { res: Req => fn2(fn(res)) }
  //    def ~^>(fn2: Res => Unit): (Req => Res) = { req: Req => sideeffect(fn(req))(fn2) }
  //    def ~+>[Res2](fn2: Req => Res => Res2): (Req => Res2) = { req: Req => fn2(req)(fn(req)) }
  //    //    def let[Mid, Res2](mid: Req => Mid)(fn2: Mid => Res => Res): Req => Res = { req: Req => fn2(mid(req))(fn(req)) }
  //    def onEnterAndExit[Mid](mid: Req => Mid, before: Mid => Unit, after: (Mid, Res) => Unit) = { req: Req =>
  //      val m = mid(req)
  //      before(m)
  //      val result = fn(req)
  //      after(m, result)
  //      result
  //    }
  //  }
  def fn[X, Y](expected: X, y: Y) = { x: X => x shouldBe expected; y }
  def fn2[X, Y, Z](expectedX: X, expectedY: Y, z: Z) = { x: X => y: Y => x shouldBe expectedX; y shouldBe expectedY; z }
  def sideeffect[X](atomicReference: AtomicReference[X]): X => Unit = atomicReference.set _
  it should "compose f1 ~> f2" in {
    (fn(1, "1") ~> fn("1", 2)) (1) shouldBe 2
  }

  it should "allow side effects to be called" in {
    val a = new AtomicReference[String]()
    (fn(1, "1") ~^> sideeffect(a)) (1) shouldBe "1"
  }

  it should "compose f ~+> g. f is A => B, g is A => B => C" in {
    (fn(1, "2") ~+> fn2(1, "2", 3)) (1) shouldBe 3
  }

  it should "perform sideeffects using on f on EnterAndExit(mid , before, after)  f is A=>B, mid is A=>M, before is Mid => Unit, after is (Mid, B) => Unit   " in {
    val before = new AtomicReference[String]
    val after = new AtomicReference[(String, String)]
    (fn(1, "2") onEnterAndExit[String](fn(1, "mid"), sideeffect(before), (m, s) => sideeffect(after)(m, s))) (1) shouldBe "2"
    before.get shouldBe "mid"
    after.get shouldBe("mid", "2")
  }

}
