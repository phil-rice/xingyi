package org.validoc.utils.concurrency

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicInteger

import org.validoc.utils.UtilsSpec

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
