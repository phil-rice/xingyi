package org.validoc.utils.concurrency

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicReference

import org.scalatest.{FlatSpec, Matchers}
import org.validoc.utils.logging.Logging


class MDCPropogatingExecutionScheduledExecutorServiceTests extends FlatSpec with Matchers with Logging {
  val defaultDaemonThreadFactory = new ThreadFactory {
    val defaultThreadFactory = Executors.defaultThreadFactory()

    def newThread(r: Runnable) = {
      val t = defaultThreadFactory.newThread(r)
      t.setDaemon(true)
      t
    }
  }

  implicit val service: MDCPropogatingExecutionScheduledExecutorService = Executors.newScheduledThreadPool(1, defaultDaemonThreadFactory)


  behavior of "MDCPropogatingExecutionContext"

  it should "take the MDC context from the calling thread and execute it in thread that does the work with runnables" in {
    setMDCvalue("key1", "value1")
    val original = copyMDC
    val actual = new AtomicReference[Map[String, String]]()
    val latch = new CountDownLatch(1)
    service.execute(new Runnable {
      override def run(): Unit = {
        actual.set(copyMDC)
        latch.countDown()
      }
    })
    latch.await()
    actual.get shouldBe original
  }
  it should "take the MDC context from the calling thread and execute it in thread that does the work with callables" in {
    setMDCvalue("key1", "value1")
    val original = copyMDC
    val actual = new AtomicReference[Map[String, String]]()
    val latch = new CountDownLatch(1)
    val result = service.submit(new Callable[String] {
      override def call(): String = {
        actual.set(copyMDC)
        latch.countDown()
        "result"
      }
    })
    latch.await()
    actual.get shouldBe original
    result.get shouldBe "result"
  }

}
