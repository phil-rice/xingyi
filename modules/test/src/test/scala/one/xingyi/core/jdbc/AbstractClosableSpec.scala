/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.jdbc

import java.util.concurrent.atomic.AtomicReference

import one.xingyi.core.closable.{ClosableLanguage, ClosableM, SimpleClosable}
import one.xingyi.core.monad.AbstractMonadTests

import scala.language.higherKinds

trait ClosableFixture {
  case class AutoClosableForTest(name: String, list: AtomicReference[List[AutoCloseable]]) extends AutoCloseable {
    override def close(): Unit = list.updateAndGet(l => l :+ this)
    override def toString: String = s"AutoclosableForTest($name)"
  }
}

abstract class AbstractClosableSpec[M[_]](name: String)(implicit val monad: ClosableM[M]) extends AbstractMonadTests[M] with ClosableLanguage with ClosableFixture {
  //  override implicit def monad: Monad[M] = ???

  behavior of name

  it should "have a close method that closes a closable lifted using the closable language" in {
    val list = new AtomicReference(List[AutoCloseable]())
    val one = AutoClosableForTest("one", list)

    val m = one.liftClosable
    list.get shouldBe List()
    m.close()
    list.get shouldBe List(one)
  }

  it should "have a close method that closes lifted closables in order" in {
    val list = new AtomicReference(List[AutoCloseable]())
    val one = AutoClosableForTest("one", list)
    val two = AutoClosableForTest("two", list)

    val m = monad.liftAndCloseAtEnd(1, Seq(one, two))
    list.get shouldBe List()
    m.close()
    list.get shouldBe List(one, two)
  }

  it should "have a close method that returns the held item - even though if this is a future this sucks" in {
    val list = new AtomicReference(List[AutoCloseable]())
    val one = AutoClosableForTest("one", list)
    val two = AutoClosableForTest("two", list)

    val m = monad.liftAndCloseAtEnd(1, Seq(one, two))
    m.close() shouldBe 1
  }

  it should "have a map method that keeps the closables" in {
    val list = new AtomicReference(List[AutoCloseable]())
    val one = AutoClosableForTest("one", list)
    val two = AutoClosableForTest("two", list)

    val m = monad.liftAndCloseAtEnd(1, Seq(one, two))
    list.get shouldBe List()
    val m2 = m.map(i => i + 1)
    list.get shouldBe List()
    m2.close()
    list.get shouldBe List(one, two)
  }

  it should "have a flatmap that aggregates closables and closes them in reverse order" in {
    val list = new AtomicReference(List[AutoCloseable]())
    val one = AutoClosableForTest("one", list)
    val two = AutoClosableForTest("two", list)

    val m1 = monad.liftAndCloseAtEnd(1, Seq(one))
    val m2 = monad.liftAndCloseAtEnd(1, Seq(two))
    val m3 = m1.flatMap(_ => m2)
    list.get shouldBe List()
    m3.close()
    list.get shouldBe List(two, one)
  }
}

class SimpleClosableSpec extends AbstractClosableSpec[SimpleClosable]("SimpleClosable") {
  override def liftA[T](t: T): SimpleClosable[T] = new SimpleClosable[T](t, Seq())
  override def getT[X](a: SimpleClosable[X]): X = a.value
}
