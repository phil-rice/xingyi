/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.language
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import one.xingyi.core.monad._
import one.xingyi.core.{FunctionFixture, UtilsSpec}
import org.mockito.Mockito._

import scala.util.{Failure, Success, Try}

class AnyLanguageSpec extends UtilsSpec with AnyLanguage with FunctionFixture {

  behavior of "use"

  it should "create a new function that makes something and then allows that thing to be used" in {
    val fn: String => String = use { input: String => input + ".made" }(made => made + ".used")

    fn("a") shouldBe "a.made.used"
  }

  behavior of "using or withValue"

  it should "allow a name to be given to a value" in {
    using("someValue")(t => t + ".used") shouldBe "someValue.used"
    withValue("someValue")(t => t + ".used") shouldBe "someValue.used"
  }
  behavior of "toSome(exists only for readability)"

  it should "turn a value into a some" in {
    toSome("hello") shouldBe Some("hello")
  }

  behavior of "sideeffect(..)"

  it should "return the original value as well as doing some sideeffect" in {
    val value = new AtomicReference[String]()
    sideeffect("a")(value.set) shouldBe "a"
    value.get shouldBe "a"
  }

  behavior of "AnyOps"

  it should "Have a |> method that passes a value to a function" in {
    1 |> fn(1, 2) shouldBe 2
  }

  it should "Have a |+> method that passes a value to a function, allowing the value to be defined as well" in {
    1 |+> { value => value shouldBe 1; fn(1, 2) } shouldBe 2
  }


  it should "have a liftM method if there is a Liftable in scope" in {
    implicit val liftable: Liftable[Option] = mock[Liftable[Option]]
    when(liftable.liftM(1)) thenReturn Some(123)
    1.liftM[Option] shouldBe Some(123)
  }

  it should "have a |? method that makes validation easy when no errors are reported" in {
    implicit val monad: MonadCanFail[Option, String] = mock[MonadCanFail[Option, String]]
    when(monad.liftM(1)) thenReturn Some(123)
    1 |? { i => Seq[String]() } shouldBe Some(123)
  }

  it should "have a |? method that makes validation easy when  errors are reported" in {
    implicit val monad: MonadCanFail[Option, String] = mock[MonadCanFail[Option, String]]
    when(monad.fail[Int]("e1e2")) thenReturn Some(123)
    1 |? { i => Seq("e1", "e2") } shouldBe Some(123)
  }

  it should "have a liftResultAndPut method if there is a Liftable in scope" in {
    implicit val monad: MonadWithState[Option] = mock[MonadWithState[Option]]
    val localVariable = LocalVariable[String]()
    when(monad.liftMAndPut(1, localVariable, "value")) thenReturn Some(123)
    1.liftResultAndPut(localVariable, "value") shouldBe Some(123)
  }

  it should "have a liftException" in {
    implicit val monad: MonadWithException[Option] = mock[MonadWithException[Option]]
    val e = new RuntimeException
    when(monad.exception[Int](e)) thenReturn Some(123)
    e.liftException shouldBe Some(123)
  }

  it should "have a ifError that returns the value if no error" in {
    1.ifError(e => throw e) shouldBe 1
  }
  it should "have a ifError that returns the result of the if error if an error occurs" in {
    val e = new RuntimeException
    def value: Int = throw e
    value.ifError(fn(e, 2)) shouldBe 2
  }

  it should "have a sideeffect try " in {
    val value = new AtomicReference[Try[Int]]()
    1.sideeffectTry(value.set) shouldBe Success(1)
    value.get shouldBe Success(1)
  }
  it should "have a sideeffect try that works with failures " in {
    val value = new AtomicReference[Try[Int]]()
    val e = new RuntimeException
    def throwError: Int = throw e
    throwError.sideeffectTry(value.set) shouldBe Failure(e)
    value.get shouldBe Failure(e)
  }

  it should "have a sideeffect" in {
    val value = new AtomicReference[Int]()
    1.sideeffect(value.set) shouldBe 1
    value.get shouldBe 1
  }

  it should "have a sideeffect that doesn't execute the sideeffect if there is a failure " in {
    val value = new AtomicReference[Boolean](false)
    val e = new RuntimeException
    def throwError: Int = throw e
    intercept[RuntimeException](throwError.sideeffect(_ => value.set(false))) shouldBe e
    value.get shouldBe false
  }

  it should "have a sideeffect that doesn't throws the exception in the sideeffect  " in {
    val e = new RuntimeException
    intercept[RuntimeException](1.sideeffect(_ => throw e)) shouldBe e
  }

  "BooleanOps" should "turn a boolean into an option" in {
    false.toOption("value") shouldBe None
    true.toOption("value") shouldBe Some("value")
  }

  "TryOps" should "lift a try into a monad when no success" in {
    implicit val monad: MonadWithException[Option] = mock[MonadWithException[Option]]
    when(monad.liftM(1)) thenReturn Some(123)
    Success(1).liftTry shouldBe Some(123)
  }
  "TryOps" should "lift a try into a monad when no failure" in {
    implicit val monad: MonadWithException[Option] = mock[MonadWithException[Option]]
    val e = new RuntimeException
    when(monad.exception[Int](e)) thenReturn Some(123)
    Failure(e).liftTry shouldBe Some(123)
  }

  "AtomicIntegerOps" should "have a tick method that executes side effect every 'tick' calls" in {
    val s = new AtomicReference("")
    val a = new AtomicInteger()
    val count = new AtomicInteger()

    def callTick = {
      a.tick(3)(s.updateAndGet(str => str + a.get + count.get()))
      count.incrementAndGet()
    }

    (1 to 10) foreach (_ => callTick)
    s.get shouldBe "020508"
    a.get() shouldBe 1
  }

  it should "have an ifNotZero that executes the function if not zero" in {
    val a = new AtomicInteger(1)
    val count = new AtomicInteger()
    a.ifNotZero(count.incrementAndGet())
    count.get shouldBe 1
  }

  it should "have an ifNotZero that doesnt execute the function if zero" in {
    val a = new AtomicInteger(0)
    val count = new AtomicInteger()
    a.ifNotZero(count.incrementAndGet())
    count.get shouldBe 0
  }

  behavior of "ListOps"

  it should "have an asstring that does a map and then a mkString" in {
    List(1, 2, 3).asString(i => (i * 2).toString) shouldBe "2,4,6"
  }

  it should "have a foldLeftWithOptions that folds unless it gets a none" in {
    List(1, 2, 3).foldLeftWithOptions("")((acc, v) =>  Some(acc + v)) shouldBe Some("123")
    List(1, 2, 3).foldLeftWithOptions("")((acc, v) =>  if (v == 3) None else Some(acc + v)) shouldBe None
    List(1, 2, 3).foldLeftWithOptions("")((acc, v) =>  if (v == 1) None else Some(acc + v)) shouldBe None
  }
  it should "have a foldLeftWithOptionsEatingExceptions that folds unless it gets a none, it returns a None if there was an exception but eats the exception" in {
    List(1, 2, 3).foldLeftWithOptionsEatingExceptions("")((acc, v) =>  Some(acc + v)) shouldBe Some("123")
    List(1, 2, 3).foldLeftWithOptionsEatingExceptions("")((acc, v) =>  if (v==2) throw new RuntimeException else Some(acc + v)) shouldBe None
    List(1, 2, 3).foldLeftWithOptionsEatingExceptions("")((acc, v) =>  if (v==2) throw new RuntimeException else if(v == 3) None else Some(acc + v)) shouldBe None
  }

}
