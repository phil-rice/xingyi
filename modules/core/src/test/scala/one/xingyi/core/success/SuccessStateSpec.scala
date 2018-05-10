package one.xingyi.core.success

import java.util.concurrent.atomic.AtomicReference

import one.xingyi.core.UtilsSpec

import scala.util.{Failure, Success, Try}

class SuccessStateSpec extends UtilsSpec {

  behavior of "SucessState"
  def setup(fn: (Try[Either[String, String]] => Unit, AtomicReference[String]) => Unit) = {
    val ref = new AtomicReference[String]()
    val state = SuccessState.sideffectWithString[String, String]((str, t) => ref.set("T" + str + t), (str, t) => ref.set("F" + str + t), (str, t) => ref.set("G" + str + t)) _
    fn(state, ref)
  }

  it should "Fold with successes" in {
    setup { (state, ref) =>
      state(Success(Right("xxx")))
      ref.get shouldBe "Gsuccessxxx"
    }
  }
  it should "Fold with failures" in {
    setup { (state, ref) =>
      state(Success(Left("fff")))
      ref.get shouldBe "Ffailedfff"
    }
  }
  it should "Fold with exceptions" in {
    setup { (state, ref) =>
      state(Failure(new RuntimeException("someMsg")))
      ref.get shouldBe "Texceptionjava.lang.RuntimeException: someMsg"
    }
  }
}
