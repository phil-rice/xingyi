package org.validoc.utils

import org.scalatest.concurrent.Eventually
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{FlatSpec, Matchers}
import scala.concurrent.duration._

import scala.concurrent.{Await, Future}

class UtilsSpec extends FlatSpec with Matchers with MockitoSugar with Eventually {

  def await[X](f: Future[X]) = Await.result(f, 5 seconds)
}