/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.script
import one.xingyi.core.http.{Body, ServiceRequest}
import one.xingyi.core.monad.MonadCanFailWithException
import one.xingyi.core.language.AnyLanguage._

import scala.language.higherKinds
case class EditEntityException(msg: String) extends RuntimeException(msg)

trait NoHostFailer[Fail] {
  def failNoHost(sr: ServiceRequest): Fail
  def failOrUseHost[M[_], T](sr: ServiceRequest)(fn: String => M[T])(implicit monad: MonadCanFailWithException[M, Fail]): M[T] = sr.header("host") match {
    case Some(host) => fn(host)
    case None => monad.fail(failNoHost(sr))
  }
}

trait EditEntityRequestFailer[Fail] extends NoHostFailer[Fail] {
  def failNoJson(sr: ServiceRequest): Fail
  def failIdDoesntMatch(id: String, sr: ServiceRequest): Fail
  def failOrUseBody[M[_], T](sr: ServiceRequest)(fn: String => M[T])(implicit monad: MonadCanFailWithException[M, Fail]): M[T] =
    sr.body match {
    case Some(Body(body)) => fn(body)
    case None => monad.fail(failNoJson(sr))
  }


}

object EditEntityRequestFailer {
  implicit object EditEntityRequestFailerForThrowable extends EditEntityRequestFailer[Throwable] {
    override def failNoJson(sr: ServiceRequest): Throwable = EditEntityException(s"No json in the request\n$sr")
    override def failNoHost(sr: ServiceRequest): Throwable = EditEntityException(s"No host in the request\n$sr")
    override def failIdDoesntMatch(id: String, sr: ServiceRequest): Throwable = EditEntityException(s"Id mismatch in url $id\n$sr")
  }
}
