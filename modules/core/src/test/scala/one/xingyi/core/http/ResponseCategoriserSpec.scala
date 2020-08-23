/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.http

import one.xingyi.core.UtilsSpec
import one.xingyi.core.exceptions.{NotFoundException, UnexpectedStatusCodeException}
import one.xingyi.core.monad.{Async, MonadCanFail}

import scala.concurrent.Future
import scala.language.higherKinds

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

import one.xingyi.core.monad.AsyncForScalaFuture.ImplicitsForTest._
import one.xingyi.core.monad.AsyncForScalaFuture._

class ScalaFutureResponseCategoriserSpec extends ResponseCategoriserSpec[Future, Throwable]
