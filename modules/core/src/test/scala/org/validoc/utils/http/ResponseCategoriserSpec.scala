package org.validoc.utils.http

import org.validoc.utils.UtilsSpec
import org.validoc.utils.exceptions.{NotFoundException, UnexpectedStatusCodeException}
import org.validoc.utils.functions.{Async, MonadCanFail}

import scala.language.higherKinds
import org.validoc.utils.language.Language._

import scala.concurrent.Future

class ResponseCategoriserSpec[M[_], Fail](implicit monadCanFail: MonadCanFail[M, Fail], failer: Failer[Fail], async: Async[M]) extends UtilsSpec {

  behavior of "default ResponseCategoriser"

  def serviceResponse(code: Int) = ServiceResponse(Status(code), Body("someBody"), ContentType("someType"))
  val request = "someString"
  val categoriser = implicitly[ResponseCategoriser[String]].categorise[Fail] apply request

  def expected(code: Int) = RequestAndServiceResponse(request, serviceResponse(code))


  it should "return a RequestAndServiceResponse for 200 status codes" in {
    categoriser(serviceResponse(200)) shouldBe Right(expected(200))
    categoriser(serviceResponse(201)) shouldBe Right(expected(201))
    categoriser(serviceResponse(204)) shouldBe Right(expected(204))
    categoriser(serviceResponse(299)) shouldBe Right(expected(299))
  }

  it should "return the failer.notFound for 404 " in {
    val Left(e: NotFoundException) = categoriser(serviceResponse(404))
    e.response shouldBe serviceResponse(404)
    e.req shouldBe request

  }
  it should "return the failer.unexpected for others " in {
    def check(code: Int) = {
      val Left(e: UnexpectedStatusCodeException) = categoriser(serviceResponse(code))
      e.response shouldBe serviceResponse(code)
      e.req shouldBe request
    }
    check(300)
    check(403)
    check(405)
    check(500)
  }
}

import org.validoc.utils.functions.AsyncForScalaFuture._
import ImplicitsForTest._

class ScalaFutureResponseCategoriserSpec extends ResponseCategoriserSpec[Future, Throwable]