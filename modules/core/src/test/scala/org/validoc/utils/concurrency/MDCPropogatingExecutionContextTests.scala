package org.validoc.utils.concurrency

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicReference

import org.scalatest.{FlatSpec, Matchers}
import org.validoc.utils.logging.Logging

class MDCPropogatingExecutionContextTests extends FlatSpec with Matchers with Logging {

  val executionContext: MDCPropagatingExecutionContext = scala.concurrent.ExecutionContext.global

  behavior of "MDCPropogatingExecutionContext"

  it should "take the MDC context from the calling thread and execute it in thread that does the work" in {
    setMDCvalue("key1", "value1")
    val original = copyMDC
    val actual = new AtomicReference[Map[String, String]]()
    val latch = new CountDownLatch(1)
    executionContext.prepare().execute(new Runnable {
      override def run(): Unit = {
        actual.set(copyMDC)
        latch.countDown()
      }
    })
    latch.await()
    actual.get shouldBe original
  }

}
