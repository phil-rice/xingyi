/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.scriptBackend2

import one.xingyi.core.http.Failer.failerForThrowable
import one.xingyi.core.logging._
import one.xingyi.core.monad.IdentityMonad
import one.xingyi.core.script.{DomainDefnToDetails, DomainList, IEntityStore}
import one.xingyi.core.simpleServer.CheapServer
import one.xingyi.json4s.Json4sParser._
import one.xingyi.json4s.Json4sWriter._
import one.xingyi.scriptModel2.{IAddress, IPerson}
import one.xingyi.scriptSharedBackend.EntityEndpoints
import org.json4s.JValue

import scala.language.higherKinds

class Backend2 {
  implicit val logger: LoggingAdapter = PrintlnLoggingAdapter

  import SimpleLogRequestAndResult._

  implicit val domainList = DomainList(DomainDefnToDetails(new Model2Defn), DomainDefnToDetails(new Model2LegacyDefn))

  implicit val personStore = IEntityStore.demo[IdentityMonad, Throwable, IPerson, Person]
  implicit val addressStore = IEntityStore.demo[IdentityMonad, Throwable, IAddress, Address]

  val websiteP = new EntityEndpoints[IdentityMonad, Throwable, JValue, IPerson, Person]
  //  val websiteA = new EntityEndpoints[IdentityMonad, Throwable, JValue, IAddress, Address]
  val backend = new CheapServer[IdentityMonad, Throwable](9001, websiteP.endpoints)

}
object Backend2 extends Backend2 with App {
  println("running")
  val server = backend.start
}

