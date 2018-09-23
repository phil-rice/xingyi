package one.xingyi.core.client

import java.io.{InputStream, OutputStream}
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
  def makeConnection(domain: Domain) = { serviceRequest: ServiceRequest => new URL(domain.asUriString + serviceRequest.uri).openConnection.asInstanceOf[HttpURLConnection] }
  def addMethod(serviceRequest: ServiceRequest, connection: HttpURLConnection) = connection.setRequestMethod(serviceRequest.method.toString)
  def addHeaders(serviceRequest: ServiceRequest, connection: HttpURLConnection) = serviceRequest.headers.foreach { h => connection.setRequestProperty(h.name, h.value) }
  def addBody(serviceRequest: ServiceRequest, connection: HttpURLConnection) = serviceRequest.body.foreach(body => StreamOps.write(connection.getOutputStream, body.s))

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
  //TODO Clean this up. Consider error stream as well...
  def makeServiceResponse[M[_] : Monad](connection: HttpURLConnection): M[ServiceResponse] = {

    val responseCode = connection.getResponseCode
    if (connection.getInputStream == null) {
      println(s"Error: " + bodyForError(connection))
    }
    val headers = connection.getHeaderFields.asScala.toList.flatMap { case (name, list) => list.asScala.map(value => Header(name, value)) }
    val body = Streams.readAll(connection.getInputStream)
    ServiceResponse(Status(connection.getResponseCode), Body(body), headers).liftM[M]
  }


  def apply[M[_] : Monad](domain: Domain)(implicit streamOps: StreamOps, sSLContext: Option[SSLContext]): ServiceRequest => M[ServiceResponse] =
    makeConnection(domain) ~+> sideeffectAll(addSslSocketFactory, addEvilSslHack, addMethod, addHeaders, addBody, connect) ~> makeServiceResponse[M] debug s"Http Client $domain $sSLContext\nRequest\n{0}\n\nResponse\n{1}"
}