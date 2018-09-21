package one.xingyi.core
import java.util.concurrent.atomic.AtomicReference

import one.xingyi.core.closable.{ClosableLanguage, ClosableM, SimpleClosable}

import scala.language.higherKinds

trait ClosableFixture {
  case class AutoClosableForTest(name: String, list: AtomicReference[List[AutoCloseable]]) extends AutoCloseable {
    override def close(): Unit = list.updateAndGet(l => l :+ this)
  }
}

class AbstractClosableSpec[M[_]](name: String)(implicit closableM: ClosableM[M]) extends UtilsSpec with ClosableLanguage with ClosableFixture {

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

    val m = closableM.liftAndCloseAtEnd(1, Seq(one, two))
    list.get shouldBe List()
    m.close()
    list.get shouldBe List(one, two)
  }
}

class SimpleClosableSpec extends AbstractClosableSpec[SimpleClosable]("SimpleClosable")