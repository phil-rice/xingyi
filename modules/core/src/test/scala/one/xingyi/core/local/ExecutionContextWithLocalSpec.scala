/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.local

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicReference

import org.scalatest.BeforeAndAfter
import one.xingyi.core.UtilsSpec

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
