package org.validoc.utils.concurrency

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicReference

import org.scalatest.{FlatSpec, Matchers}
import org.validoc.utils.UtilsSpec
import org.validoc.utils.logging.Logging

import scala.concurrent.Future

class MDCPropogatingExecutionContextTests extends UtilsSpec with Logging {


  behavior of "MDCPropogatingExecutionContext"

  it should "take the MDC context from the calling thread and execute it in thread that does the work" in {
    setMDCvalue("key1", "value1")
    val original = copyMDC
    val actual = new AtomicReference[Map[String, String]]()
    val latch = new CountDownLatch(1)
    ec.prepare().execute(new Runnable {
      override def run(): Unit = {
        actual.set(copyMDC)
        latch.countDown()
      }
    })
    latch.await()
    actual.get shouldBe original
  }

  it should "work with the Future{block} code" in {
    setMDCvalue("key1", "value1")
    val original = copyMDC
    val actual = new AtomicReference[Map[String, String]]()
    val latch = new CountDownLatch(1)
    Future {
      actual.set(copyMDC)
      latch.countDown()
    }
    latch.await()
    actual.get shouldBe original
  }

  it should "restore the MDC at the end" in {
    setMDCvalue("key1", "value1")
    val original = copyMDC
    await(Future(clearMdc))
    getMDCvalue("key1").get shouldBe "value1"
  }

}
