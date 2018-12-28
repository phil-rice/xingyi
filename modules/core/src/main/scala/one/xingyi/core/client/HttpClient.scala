/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.client

import java.io.{FileNotFoundException, IOException, InputStream, OutputStream}
import java.net.{HttpURLConnection, URL}

import javax.net.ssl.{HttpsURLConnection, SSLContext}
import one.xingyi.core.http._
import one.xingyi.core.language.Language._
import one.xingyi.core.monad.Monad
import one.xingyi.core.simpleServer.Streams

import scala.collection.JavaConverters._
import scala.language.higherKinds

trait StreamOps {
  def write(stream: OutputStream, s: String): Unit = try {
    stream.write(s.getBytes("UTF-8"))
  } finally (stream.close())
}

object StreamOps extends StreamOps {
  implicit val default: StreamOps = this
}


object HttpClient {
  def makeConnection(domain: Domain) = { serviceRequest: ServiceRequest =>
    require(Some(domain) == serviceRequest.uri.domain, serviceRequest.uri + " " + serviceRequest.uri.domain + " != " + domain)
    new URL(serviceRequest.uri.asUriString).openConnection.asInstanceOf[HttpURLConnection]
  }

  def addMethod(serviceRequest: ServiceRequest, connection: HttpURLConnection) = {
    connection.setRequestMethod(serviceRequest.method.toString.toUpperCase)
  }

  def addHeaders(serviceRequest: ServiceRequest, connection: HttpURLConnection) = serviceRequest.headers.foreach { h => connection.setRequestProperty(h.name, h.value) }

  def addBody(serviceRequest: ServiceRequest, connection: HttpURLConnection) = serviceRequest.body.foreach { body => connection.setDoOutput(true); StreamOps.write(connection.getOutputStream, body.s) }

  def addSslSocketFactory(serviceRequest: ServiceRequest, connection: HttpURLConnection)(implicit sslContext: Option[SSLContext]) =
    sslContext.foreach(ssl => connection.sideeffectIfIs[HttpsURLConnection] { h: HttpsURLConnection => h.setSSLSocketFactory(ssl.getSocketFactory) })

  def addEvilSslHack(serviceRequest: ServiceRequest, connection: HttpURLConnection) =
    connection sideeffectIfIs[HttpsURLConnection] { u: HttpsURLConnection => u.setHostnameVerifier((a, b) => true) }

  def connect(serviceRequest: ServiceRequest, connection: HttpURLConnection) =
    connection.connect().ifError(e => throw UrlDidntResponse(serviceRequest.uri.asUriString, e))


  def bodyForError(connection: HttpURLConnection) = {
    def get(stream: => InputStream) = try {
      if (stream == null) "<null>" else Streams.readAll(stream)
    } catch {
      case e: Exception => s"Error: $e"
    }

    s"Normal:\n${get(connection.getInputStream)} \n\nError:\n${get(connection.getErrorStream)}"
  }

  def bodyFromErrorStream(connection: HttpURLConnection): Body = if (connection.getErrorStream == null) Body("") else Body(bodyForError(connection))

  //TODO Clean this up. Consider error stream as well...
  def makeServiceResponse[M[_] : Monad](connection: HttpURLConnection): M[ServiceResponse] = try {

    val responseCode = connection.getResponseCode
    if (connection.getInputStream == null) throw new NullPointerException(s"connection.inputstream is null: " + bodyForError(connection))

    val headers = connection.getHeaderFields.asScala.toList.flatMap { case (name, list) => list.asScala.map(value => Header(name, value)) }
    val body = Streams.readAll(connection.getInputStream)
    ServiceResponse(Status(connection.getResponseCode), Body(body), headers).liftM[M]
  } catch {
    case e: FileNotFoundException => ServiceResponse(Status(404), bodyFromErrorStream(connection), List()).liftM[M]
    case e: IOException => ServiceResponse(Status(500), bodyFromErrorStream(connection), List()).liftM[M]
  }


  def apply[M[_] : Monad](domain: Domain)(implicit streamOps: StreamOps, sSLContext: Option[SSLContext]): ServiceRequest => M[ServiceResponse] =
    makeConnection(domain) ~+> sideeffectAll(addSslSocketFactory, addEvilSslHack, addMethod, addHeaders, addBody, connect) ~> makeServiceResponse[M] //debug s"Http Client $domain $sSLContext\nRequest\n{0}\n\nResponse\n{1}"
}
