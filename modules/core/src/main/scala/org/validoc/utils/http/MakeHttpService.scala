package org.validoc.utils.http

import org.validoc.utils.service.RootServiceDescription

import scala.language.higherKinds
import scala.reflect.ClassTag


trait MakeHttpService[M[_], HttpReq, HttRes] extends (ProtocolHostAndPort => (HttpReq => M[HttRes])) {
  def create(hostName: HostName, port: Port) = apply(ProtocolHostAndPort(hostName, port))
}

trait MakeHttpServiceLanguage[M[_], HttpReq, HttpRes] {
  def http(hostName: HostName, port: Port)(implicit makeHttpService: MakeHttpService[M, HttpReq, HttpRes], httpReqClassTag: ClassTag[HttpReq], httpResClassTag: ClassTag[HttpRes]) =
    new RootServiceDescription(ProtocolHostAndPort(hostName, port), makeHttpService)
}

object MakeHttpService {
  def apply[M[_], HttpReq, HttpRes](map: Map[String, (HttpReq => M[HttpRes])]) = new MakeHttpService[M, HttpReq, HttpRes] {
    override def apply(protocolHostAndPort: ProtocolHostAndPort): (HttpReq) => M[HttpRes] = map(protocolHostAndPort.hostName.host)
  }

  /** This exists  to allow the 'to string' interpreter to work */
  implicit object MakeHttpServiceForString extends MakeHttpService[Option, String, String] {
    override def apply(protocolHostAndPort: ProtocolHostAndPort): (String) => Option[String] = req => Some(s"HttpService($protocolHostAndPort)($req)")
  }

}