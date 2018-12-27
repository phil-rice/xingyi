/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.scriptBackend3

import one.xingyi.core.http.Failer.failerForThrowable
import one.xingyi.core.http._
import one.xingyi.core.json._
import one.xingyi.core.logging._
import one.xingyi.core.monad.{Async, IdentityMonad, MonadCanFailWithException}
import one.xingyi.json4s.Json4sParser._
import one.xingyi.json4s.Json4sWriter._
import one.xingyi.scriptModel3.IPerson
import one.xingyi.scriptSharedBackend.SharedBackend
import org.json4s.JValue

import scala.language.higherKinds

class Backend3[M[_] : Async, Fail: Failer : LogRequestAndResult, J: JsonParser : JsonWriter, SharedP, DomainP]
(implicit monadCanFailWithException: MonadCanFailWithException[M, Fail], loggingAdapter: LoggingAdapter)
  extends SharedBackend[M, Fail, J, IPerson, Person](new Model3Defn) {
  override def makeNewPerson(name: String): Person = Person("someName", List(Address("someLine1", "someLine2", "somePostCode")), Telephone("someTelephoneNumber"))

  override def findName(p: Person): String = p.name

  override def person: Person = makeNewPerson("somePerson")
}

object Backend3 extends App {
  implicit val logger: LoggingAdapter = PrintlnLoggingAdapter

  import SimpleLogRequestAndResult._

  val backend = new Backend3[IdentityMonad, Throwable, JValue, IPerson, Person]

  println("running")
  backend.start
}
