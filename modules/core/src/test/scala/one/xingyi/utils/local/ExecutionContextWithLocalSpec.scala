package one.xingyi.utils.local

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicReference

import org.scalatest.BeforeAndAfter
import one.xingyi.utils.UtilsSpec

import scala.concurrent.{ExecutionContext, Future}
import org.mockito.Mockito._

class ExecutionContextWithLocalSpec extends UtilsSpec with LocalOpsPimper[Future] with BeforeAndAfter {


  behavior of "ExecutionContextWithLocal"

  it should "propogate the 'local' across execution" in {
    val executionContextWithLocal = new ExecutionContextWithLocal(ExecutionContext.Implicits.global)
    val atomicReference = new AtomicReference[Option[String]]()
    putInlocalStore[String]("123")
    val countDownLatch = new CountDownLatch(1)
    executionContextWithLocal.execute { () =>
      atomicReference.set(getFromLocalStore[String])
      countDownLatch.countDown()
    }
    countDownLatch.await()
    atomicReference.get shouldBe Some("123")
  }

  it should "have a report failure that delegates" in {
    val ec = mock[ExecutionContext]
    val executionContextWithLocal = new ExecutionContextWithLocal(ec)
    val runtimeException = new RuntimeException
    executionContextWithLocal.reportFailure(runtimeException)
    verify(ec, times(1)).reportFailure(runtimeException)
  }

}
