package org.validoc.utils.time

import java.util.concurrent.atomic.{AtomicLong, AtomicReference}

import org.scalatest.concurrent.Eventually
import org.scalatest.mock.MockitoSugar
import org.scalatest.{FlatSpec, Matchers}
import org.validoc.utils.concurrency.MDCPropagatingExecutionContext

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

import org.validoc.utils._
import org.validoc.utils.concurrency.AsyncForScalaFuture._

class MockTimeService extends NanoTimeService {
  val i = new AtomicLong(1000)

  override def apply(): Long = i.addAndGet(100l)
}


