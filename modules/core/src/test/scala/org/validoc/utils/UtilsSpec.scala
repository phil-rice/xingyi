package org.validoc.utils

import org.scalatest.concurrent.Eventually
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{FlatSpec, Matchers}
import org.validoc.utils.concurrency.Async

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Try

class UtilsSpec extends FlatSpec with Matchers with MockitoSugar with Eventually {

  def await[X](f: Future[X]) = Await.result(f, 5 seconds)

  implicit class UtilsSpecAsyncPimper[M[_], T](mt: M[T])(implicit async: Async[M]) {
    def await = async.await(mt, 5 seconds)
  }

}