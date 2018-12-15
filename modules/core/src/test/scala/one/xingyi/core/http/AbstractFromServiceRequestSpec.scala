/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.http
import one.xingyi.core.UtilsSpec
import one.xingyi.core.language.Language._
import one.xingyi.core.monad.{Async, Monad}

import scala.language.higherKinds
import scala.reflect.ClassTag

abstract class AbstractFromServiceRequestSpec[M[_] : Async, Res: ClassTag](implicit monad: Monad[M], fromServiceRequest: FromServiceRequest[M, Res]) extends UtilsSpec {

  def checkMethod(res: Res, sr: ServiceRequest)
  def checkHeaders(res: Res, sr: ServiceRequest)
  def checkBody(res: Res, sr: ServiceRequest)
  def checkUri(res: Res, sr: ServiceRequest)

  val headers = List(Header("one", "valueOne"))

  val sr = ServiceRequest(Get, Uri("/someUri?a=1&b=2"), headers, Some(Body("body")))
  val srNoBody = ServiceRequest(Get, Uri("/someUri?a=1&b=2"), headers, None)

  behavior of "FromServiceRequest for " + implicitly[ClassTag[Res]].runtimeClass.getName

  it should "turn a service request into a Res - with body" in {
    val res = fromServiceRequest(sr).await()
    checkMethod(res, sr)
    checkHeaders(res, sr)
    checkBody(res, sr)
    checkUri(res, sr)
  }

  it should "turn a service request into a Res - with out body" in {
    val res = fromServiceRequest(srNoBody).await()
    checkMethod(res, srNoBody)
    checkHeaders(res, srNoBody)
    checkBody(res, srNoBody)
    checkUri(res, srNoBody)
  }
}
