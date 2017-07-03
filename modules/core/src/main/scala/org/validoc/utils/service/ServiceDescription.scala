package org.validoc.utils.service


import org.validoc.utils.http.{HostName, MakeHttpService, Port}

import scala.reflect.ClassTag
import scala.language.higherKinds

trait ServiceReporter[Service] extends (Service => Option[String])

object ServiceReporter {
  implicit def defaultServiceReporter[Service] = new ServiceReporter[Service] {
    override def apply(v1: Service): Option[String] = None
  }
}

abstract class ServiceDescription[M[_], Req, Res](implicit serviceReporter: ServiceReporter[Req => M[Res]]) {
  def service: Req => M[Res]

  def report: Option[String] = serviceReporter(service)

  def fold[T](folder: ServiceDescriptionFolder[M, T])(initial: T): T

  def description: String
}


case class RootHttpServiceDescription[M[_], HttpReq, HttpRes](hostName: HostName, port: Port, makeHttpService: MakeHttpService[M, HttpReq, HttpRes])
                                                             (implicit serviceReporter: ServiceReporter[HttpReq => M[HttpRes]])
  extends ServiceDescription[M, HttpReq, HttpRes] {

  lazy val service = makeHttpService.create(hostName, port)

  override def fold[T](folder: ServiceDescriptionFolder[M, T])(initial: T): T = folder.root(initial, this)

  override def description = s"RootHttp(${hostName},$port)"
}

case class DelegateServiceDescription[M[_], OldReq, OldRes, Req, Res, Service <: Req => M[Res] : ClassTag]
(delegate: ServiceDescription[M, OldReq, OldRes], serviceMaker: MakeServiceMakerForClass[OldReq => M[OldRes], Service])
(implicit serviceReporter: ServiceReporter[Service]) extends ServiceDescription[M, Req, Res] {

  val serviceClass = implicitly[ClassTag[Service]].runtimeClass

  lazy val service = serviceMaker(delegate.service)

  override def fold[T](folder: ServiceDescriptionFolder[M, T])(initial: T): T = delegate.fold(folder)(folder.service(initial, this))

  override def description = serviceClass.getSimpleName
}

case class ParamDelegateServiceDescription[M[_], Param, OldReq, OldRes, Req, Res, Service <: Req => M[Res] : ClassTag]
(param: Param, delegate: ServiceDescription[M, OldReq, OldRes], serviceMaker: MakeServiceMakerForClassWithParam[Param, OldReq => M[OldRes], Service])
(implicit serviceReporter: ServiceReporter[Service]) extends ServiceDescription[M, Req, Res] {

  val serviceClass = implicitly[ClassTag[Service]].runtimeClass

  lazy val service = serviceMaker(param, delegate.service)

  override def fold[T](folder: ServiceDescriptionFolder[M, T])(initial: T): T = delegate.fold(folder)(folder.serviceWithParam(initial, this))

  override def description = s"${serviceClass.getSimpleName}($param)"
}

trait MakeServiceDescription[M[_], OldReq, OldRes, Req, Res] {
  def apply(delegate: ServiceDescription[M, OldReq, OldRes]): ServiceDescription[M, Req, Res]
}