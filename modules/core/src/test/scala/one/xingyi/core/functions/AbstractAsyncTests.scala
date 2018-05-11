package one.xingyi.core.functions

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import org.scalatest.BeforeAndAfter
import one.xingyi.core.UtilsWithLoggingSpec
import one.xingyi.core.http.Failer
import one.xingyi.core.language.MonadLanguage
import one.xingyi.core.reflection.ClassTags

import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.language.{higherKinds, postfixOps}
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

trait ContainerSpec[A[_]] extends UtilsWithLoggingSpec {

  def liftA[T](t: T): A[T]
  def getT[X](a: A[X]): X
  def checkHasException[E <: Exception : ClassTag](a: A[String]): E = intercept[E](getT(a))
  val runtimeException = new RuntimeException("someMessage")

}

trait ScalaFutureAsAsyncAndMonadAndFailer {

  import AsyncForScalaFuture.ImplicitsForTest._

  private val a = AsyncForScalaFuture.defaultAsyncForScalaFuture
  implicit protected def async: Async[Future] = a
  implicit protected def monad: MonadCanFailWithException[Future, Throwable] = a
  implicit protected def failer = implicitly[Failer[Throwable]]
}

trait AbstractAsyncTests[A[_]] extends ContainerSpec[A] {
  def async: Async[A]
  override def getT[X](a: A[X]) = async.await(a)


  behavior of s"Async for ${async.getClass.getSimpleName}"

  it should "lift values into A which can be awaited for" in {
    getT(async.async("someString")) shouldBe "someString"
  }

  it should "lift exceptions into A which can be awaited for" in {
    val a: A[String] = async.async(throw runtimeException)
    intercept[RuntimeException](getT(a)) shouldBe runtimeException
  }

  it should "have a respond method that is called after the monad gains a value" in {
    val count = new AtomicInteger()
    val remembered = new AtomicReference[Try[String]]()
    getT(async.respond[String](async.async("someString"), { tryR =>
      remembered.set(tryR)
      count.incrementAndGet()
    })) shouldBe "someString"
    eventually(count.get shouldBe 1)
    remembered.get shouldBe Success("someString")
  }
  it should "have a respond method that is called after the monad throws an exception" in {
    val count = new AtomicInteger()
    val remembered = new AtomicReference[Try[String]]()
    val a: A[String] = async.async[String](throw runtimeException)
    intercept[RuntimeException](getT(async.respond[String](a, { tryR =>
      remembered.set(tryR)
      count.incrementAndGet()
    }))) shouldBe runtimeException
    eventually(count.get shouldBe 1)
    remembered.get shouldBe Failure(runtimeException)
  }


  it should "async values into A on a different thread" in {
    val thisThread = Thread.currentThread().getName
    getT(async.async(Thread.currentThread().getName)) shouldNot be(thisThread)
  }
}

trait AbstractFunctorTests[A[_]] extends ContainerSpec[A] {
  implicit def functor: Functor[A]

  behavior of s"Functor for ${functor.getClass.getSimpleName}"

  it should "execute map" in {
    getT(functor.map[String, String](liftA("first"), x => x + "second")) shouldBe "firstsecond"
  }
}

trait AbstractMonadTests[A[_]] extends AbstractFunctorTests[A] with MonadLanguage {
  implicit def monad: Monad[A]
  override def functor: Monad[A] = monad

  behavior of s"Monad for ${monad.getClass.getSimpleName}"

  it should "lift values into A" in {
    getT(monad.liftM("someString")) shouldBe "someString"
  }

  it should "execute flatMap" in {
    getT(monad.flatMap[String, String](monad.liftM("first"), x => monad.liftM(x + "second"))) shouldBe "firstsecond"
  }
  it should "execute join2" in {
    getT(join2(liftA(1), liftA(2))) shouldBe(1, 2)
  }

  it should "execute join3" in {
    getT(join3(liftA(1), liftA(2), liftA(3))) shouldBe(1, 2, 3)
  }

  it should "execute join4" in {
    getT(join4(liftA(1), liftA(2), liftA(3), liftA(4))) shouldBe(1, 2, 3, 4)
  }
  it should "execute join5" in {
    getT(join5(liftA(1), liftA(2), liftA(3), liftA(4), liftA(5))) shouldBe(1, 2, 3, 4, 5)
  }

}

abstract class AbstractMonadWithExceptionTests[A[_]](implicit m: MonadWithException[A]) extends AbstractMonadTests[A] {
  def liftTryA[T](t: Try[T]): A[T] = m.liftTry(t)
  def liftA[T](t: T): A[T] = liftTryA(Success(t))


  it should "execute map when an exception occurs in the functor" in {
    val runtimeException = new RuntimeException
    val a = functor.map(liftTryA(Failure[String](runtimeException)), { x: String => x + 1 })
    checkHasException[RuntimeException](a) shouldBe runtimeException
  }
  it should "execute map when an exception occurs in the fn" in {
    val runtimeException = new RuntimeException
    val a = functor.map[String, String](liftTryA(Success("1")), { x: String => throw runtimeException })
    checkHasException[RuntimeException](a) shouldBe runtimeException
  }

  it should "execute flatMap when a exception in monad" in {
    val a = functor.flatMap[String, String](liftTryA(Failure[String](runtimeException)), { x: String => monad.liftM("a") })
    checkHasException[RuntimeException](a) shouldBe runtimeException
  }

  it should "execute flatMap when an exception occurs in the fn" in {
    val a = functor.flatMap[String, String](liftTryA(Success("1")), { x: String => throw runtimeException })
    checkHasException[RuntimeException](a) shouldBe runtimeException
  }


  behavior of s"monad with exception ${getClass.getSimpleName}"

  it should "be able to lift an exception" in {
    checkHasException[RuntimeException](m.exception(runtimeException)) shouldBe runtimeException
  }

  it should "liftTry into A for successes" in {
    getT(m.liftTry(Success("someString"))) shouldBe "someString"
  }

  it should "liftTry into A for failures" in {
    checkHasException[RuntimeException](m.liftTry(Failure(runtimeException))) shouldBe runtimeException
  }

  it should "recover from exceptions" in {
    getT(m.recover(liftTryA(Failure(runtimeException)), x => m.liftM(x.getMessage))) shouldBe "someMessage"
  }
}

trait MonadCanFailFixture[A[_], Fail] {

  def makeFail(s: String): Fail
  def failToString(f: Fail): String
}

//only extended by those that don't have MonadCanFailWithException.
abstract class AbstractMonadCanFailTests[A[_], Fail: ClassTag](implicit monad: MonadCanFail[A, Fail]) extends AbstractMonadTests[A] with MonadCanFailFixture[A, Fail] {
  behavior of s"monad can fail ${getClass.getSimpleName} with fail being ${ClassTags.nameOf[Fail]}"

  it should "have a mapEither that works when holds a T" in {
    val remember = new AtomicReference[Either[Fail, String]]()
    getT(monad.mapEither[String, String](monad.liftM("someValue"), { e: Either[Fail, String] => remember.set(e); monad.liftM("result") })) shouldBe "result"
    remember.get shouldBe Right("someValue")
  }
  it should "have a mapEither that works when holds a Fail" in {
    val remember = new AtomicReference[Either[Fail, String]]()
    getT(monad.mapEither[String, String](monad.fail(makeFail("someValue")), { e: Either[Fail, String] => remember.set(e); monad.liftM("result") })) shouldBe "result"
    remember.get shouldBe Left("someValue")
  }

}

abstract class AbstractMonadCanFailWithExceptionTests[A[_], Fail: ClassTag](implicit monad: MonadCanFailWithException[A, Fail]) extends AbstractMonadWithExceptionTests[A] with MonadCanFailFixture[A, Fail] {
  behavior of s"monad can fail with exception${getClass.getSimpleName} with fail being ${ClassTags.nameOf[Fail]}"


  def callFold(a: A[String]) = getT(monad.foldWithExceptionAndFail[String, String](a, e => monad.liftM(e.getMessage), f => monad.liftM(failToString(f)), x => monad.liftM(x + "happened")))


  it should "work with fold value" in {
    callFold(liftA("A")) shouldBe "Ahappened"
  }

  it should "work with fold exception" in {
    callFold(liftTryA(Failure(runtimeException))) shouldBe "someMessage"
  }
  it should "work with fold failure" in {
    callFold(monad.fail(makeFail("fail"))) shouldBe "fail"
  }

  behavior of s"monad can fail ${getClass.getSimpleName} with fail being ${ClassTags.nameOf[Fail]}"

  it should "have a mapEither that works when holds a T" in {
    val remember = new AtomicReference[Either[Fail, String]]()
    getT(monad.mapEither[String, String](monad.liftM("someValue"), { e: Either[Fail, String] => remember.set(e); monad.liftM("result") })) shouldBe "result"
    remember.get shouldBe Right("someValue")
  }


  def callTry(value: A[String]): Try[Either[Fail, String]] = {
    val remember = new AtomicReference[Try[Either[Fail, String]]]()
    val count = new AtomicInteger()
    try {
      getT(monad.mapTryFail(value, { x: Try[Either[Fail, String]] => remember.set(x); count.incrementAndGet(); liftA("result") }))
    } catch {
      case e: Exception =>
    }
    count.get shouldBe 1
    remember.get
  }

  def callOnComplete(value: A[String]): Try[Either[Fail, String]] = {
    val remember = new AtomicReference[Try[Either[Fail, String]]]()
    val count = new AtomicInteger()
    Try(getT(monad.onComplete(value, { x: Try[Either[Fail, String]] => remember.set(x); count.incrementAndGet(); liftA("shouldNotBeUsed") })))
    count.get shouldBe 1
    remember.get
  }

}

abstract class AbstractMonadCanFailWithFailWithExceptionAsThrowableTests[A[_]](implicit monad: MonadCanFailWithException[A, Throwable]) extends AbstractMonadCanFailWithExceptionTests[A, Throwable] {
  override def makeFail(s: String) = new RuntimeException(s)
  override def failToString(f: Throwable) = f.getMessage

  def checkException(tryS: Try[Either[Throwable, String]]) = {
    val Failure(e: RuntimeException) = tryS
    e.getMessage shouldBe "someMessage"
  }
  it should "have a mapTryFail method which prefers Left to Failure" in {
    callTry(liftA("someValue")) shouldBe Success(Right("someValue"))
    checkException(callTry(monad.fail(makeFail("someMessage"))))
    checkException(callTry(monad.exception(runtimeException)))
  }
  it should "have a onComplete method which prefers Left to Failure" in {
    callOnComplete(liftA("someValue")) shouldBe Success(Right("someValue"))
    checkException(callOnComplete(monad.fail(makeFail("someMessage"))))
    checkException(callOnComplete(monad.exception(runtimeException)))
  }
  it should "have a mapEither that works when holds a Throwable" in {
    val remember = new AtomicReference[Either[Throwable, String]]()

    val m = monad.mapEither[String, String](monad.exception(runtimeException), { e: Either[Throwable, String] => remember.set(e); monad.liftM("result") })
    getT(m) shouldBe "result"
    remember.get shouldBe Left(runtimeException)
  }

}

abstract class AbstractMonadCanFailWithFailWithExceptionNotAsThrowableTests[A[_], Fail: ClassTag](implicit monad: MonadCanFailWithException[A, Fail]) extends AbstractMonadCanFailWithExceptionTests[A, Fail] {
  it should "have a mapTryFail method " in {
    callTry(liftA("someValue")) shouldBe Success(Right("someValue"))
    callTry(monad.fail(makeFail("someValue"))) shouldBe Success(Left(makeFail("someValue")))
    callTry(monad.exception(runtimeException)) shouldBe Failure(runtimeException)
  }
  it should "have a mapEither that works when holds a Fail" in {
    val remember = new AtomicReference[Either[Fail, String]]()
    getT(monad.mapEither[String, String](monad.fail(makeFail("someValue")), { e: Either[Fail, String] => remember.set(e); monad.liftM("result") })) shouldBe "result"
    remember.get shouldBe Left("someValue")
  }
  it should "have a mapEither that works when holds a Throwable" in {
    val remember = new AtomicReference[Either[Fail, String]]()

    val m = monad.mapEither[String, String](monad.exception(runtimeException), { e: Either[Fail, String] => remember.set(e); monad.liftM("result") })
    intercept[RuntimeException](getT(m)) shouldBe runtimeException
    remember.get shouldBe null
  }

  it should "have a onComplete method " in {
    callOnComplete(liftA("someValue")) shouldBe Success(Right("someValue"))
    callOnComplete(monad.fail(makeFail("someValue"))) shouldBe Success(Left(makeFail("someValue")))
    callOnComplete(monad.exception(runtimeException)) shouldBe Failure(runtimeException)
  }

  it should "have a flatMap that returns the fail" in {
    callOnComplete(monad.flatMap[String, String](monad.fail(makeFail("someValue")), _ => throw new RuntimeException)) shouldBe Success(Left(makeFail("someValue")))
  }


}
