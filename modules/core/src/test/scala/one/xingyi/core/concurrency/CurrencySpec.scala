/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.concurrency

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicInteger

import one.xingyi.core.UtilsSpec

class CurrencySpec extends UtilsSpec {

  behavior of "DoubleChecked lock"

  def setupDoubleCheckedLock(cond: Boolean)(fn: (CountDownLatch, DoubleCheckLock, (() => Boolean), AtomicInteger) => Unit) = {
    val conditionLatch = new CountDownLatch(1)
    val count = new AtomicInteger()
    val condition: () => Boolean = { () =>
      conditionLatch.await()
      cond
    }
    fn(conditionLatch, new DoubleCheckLock, condition, new AtomicInteger())
  }


  it should "not execute the box if the condition is false" in {

    setupDoubleCheckedLock(false) { (condCountDown, lock, condition, count) =>
      condCountDown.countDown()
      lock(condition())(count.incrementAndGet())
      count.get() shouldBe 0
    }
  }

  it should "execute the box if there is no contention and the condition is true" in {
    setupDoubleCheckedLock(true) { (condCountDown, lock, condition, count) =>
      condCountDown.countDown()
      lock(condition())(count.incrementAndGet())
      count.get() shouldBe 1
    }
  }

  //TODO I should do some threading tests here
}
