/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.http

import javax.net.ssl.SSLContext
import one.xingyi.core.client.HttpClient
import one.xingyi.core.monad.Monad

import scala.language.higherKinds

trait HttpFactory[M[_], HttpReq, HttpRes] extends (ServiceName => HttpReq => M[HttpRes])
object HttpFactory {
  def apply[M[_] : Monad](nameAndDomain: (String, Domain)*)(implicit ssLContext: Option[SSLContext]): HttpFactory[M, ServiceRequest, ServiceResponse] = {
    val map = nameAndDomain.map(kv => ServiceName(kv._1) -> kv._2).toMap;
    { serviceName =>
      val domain = map.getOrElse(serviceName, throw new IllegalArgumentException(s"Service name $serviceName"))
      val service = HttpClient.apply[M](domain);
      { req => service(req.copy(domain = Some(domain))) }
    }
  }
}

trait HttpKlesili[M[_]] {
  protected def httpFactory: HttpFactory[M, ServiceRequest, ServiceResponse]
  def http(name: ServiceName): ServiceRequest => M[ServiceResponse] = httpFactory(name)

}
