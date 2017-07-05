package org.validoc.utils.http

import org.validoc.utils.service.RootHttpServiceDescription

import scala.language.higherKinds

trait MakeHttpService[M[_], HttpReq, HttRes] {
  def create(hostName: HostName, port: Port): (HttpReq => M[HttRes])
}

trait MakeHttpServiceLanguage[M[_], HttpReq, HttpRes] {
  def http(hostName: HostName, port: Port)(implicit makeHttpService: MakeHttpService[M, HttpReq, HttpRes]) =
    new RootHttpServiceDescription(hostName, port, makeHttpService)
}

object MakeHttpService {
  def apply[M[_], HttpReq, HttpRes](map: Map[String, (HttpReq => M[HttpRes])]) = new MakeHttpService[M, HttpReq, HttpRes] {
    override def create(hostName: HostName, port: Port): (HttpReq) => M[HttpRes] = map(hostName.host)
  }

  /** This exists  to allow the 'to string' interpreter to work */
  implicit object MakeHttpServiceForString extends MakeHttpService[Option, String, String] {
    override def create(hostName: HostName, port: Port): (String) => Option[String] = req => Some(s"HttpService($req)")
  }

}