package org.validoc.utils.service


import org.validoc.utils.concurrency.Async
import org.validoc.utils.functions.SemiGroup
import org.validoc.utils.http.{HostName, MakeHttpService, Port}
import org.validoc.utils.registry.ObjectRegistry
import org.validoc.utils.strings.HasShortToString

import scala.language.higherKinds
import scala.reflect.ClassTag

trait ServiceReporter[Service] extends (Service => Option[String])

object ServiceReporter {
  implicit def defaultServiceReporter[Service] = new ServiceReporter[Service] {
    override def apply(v1: Service): Option[String] = None
  }
}

object ServiceDescription {
  def all[M[_], T](implicit classTag: ClassTag[T]) = ObjectRegistry.all[ServiceDescription[M]].collect {
    case n: NamedServiceDescription[M, _, _, _] if n.serviceClass == classTag.runtimeClass => n
  }
}

trait ServiceDescription[M[_]] extends ObjectRegistry[ServiceDescription[M]] with HasShortToString {
  type FoldFn[T] = (AbstractServiceDescription[M, _, _], Int) => T

  def fold[T](fn: FoldFn[T], depth: Int)(implicit group: SemiGroup[T]): T

  def report: Option[String]

  def children: List[AbstractServiceDescription[M, _, _]]
}

abstract class AbstractServiceDescription[M[_], Req: ClassTag, Res: ClassTag](implicit serviceReporter: ServiceReporter[Req => M[Res]]) extends ServiceDescription[M] {
  def service: (Req => M[Res])

  def report: Option[String] = serviceReporter(service)

  def fold[T](fn: FoldFn[T], depth: Int)(implicit group: SemiGroup[T]): T = {
    val thisResult = fn(this, depth)
    children.map(_.fold(fn, depth + 1)) match {
      case head :: tail => {
        group.add(thisResult, group.add(head, tail))
      }
      case _ => thisResult
    }
  }
}

case class RootHttpServiceDescription[M[_], HttpReq: ClassTag, HttpRes: ClassTag](hostName: HostName, port: Port, makeHttpService: MakeHttpService[M, HttpReq, HttpRes])
                                                                                 (implicit serviceReporter: ServiceReporter[HttpReq => M[HttpRes]])
  extends AbstractServiceDescription[M, HttpReq, HttpRes] {

  lazy val service = makeHttpService.create(hostName, port)

  override def shortToString = s"${registryId}:RootHttp(${hostName},$port)"

  override def children: List[AbstractServiceDescription[M, _, _]] = List()
}

abstract class NamedServiceDescription[M[_], Req: ClassTag, Res: ClassTag, Service <: Req => M[Res] : ClassTag] extends AbstractServiceDescription[M, Req, Res] {
  def nameOf[T](implicit classTag: ClassTag[T]) = classTag.runtimeClass.getSimpleName

  val serviceClass = implicitly[ClassTag[Service]].runtimeClass

  override def shortToString = s"${registryId}:${nameOf[Service]}[${nameOf[Req]},${nameOf[Res]}]"

}

case class DelegateServiceDescription[M[_], OldReq: ClassTag, OldRes: ClassTag, Req: ClassTag, Res: ClassTag, Service <: Req => M[Res] : ClassTag]
(delegate: AbstractServiceDescription[M, OldReq, OldRes], serviceMaker: (OldReq => M[OldRes]) => Service)
(implicit serviceReporter: ServiceReporter[Service]) extends NamedServiceDescription[M, Req, Res, Service] {

  lazy val service = serviceMaker(delegate.service)


  override def children: List[AbstractServiceDescription[M, _, _]] = List(delegate)
}

case class ParamDelegateServiceDescription[M[_], Param, OldReq: ClassTag, OldRes: ClassTag, Req: ClassTag, Res: ClassTag, Service <: Req => M[Res] : ClassTag]
(param: Param, delegate: AbstractServiceDescription[M, OldReq, OldRes], serviceMaker: (Param, OldReq => M[OldRes]) => Service)
(implicit serviceReporter: ServiceReporter[Service]) extends NamedServiceDescription[M, Req, Res, Service] {


  lazy val service = serviceMaker(param, delegate.service)


  override def shortToString = s"${super.shortToString}($param)"

  override def children: List[AbstractServiceDescription[M, _, _]] = List(delegate)
}


case class MergingTwoServicesDescription[M[_] : Async, Req1, Res1, Req2, Res2, Req: ClassTag, Res: ClassTag, Service <: Req => M[Res] : ClassTag]
(service1: AbstractServiceDescription[M, Req1, Res1],
 service2: AbstractServiceDescription[M, Req2, Res2],
 maker: (Req1 => M[Res1], Req2 => M[Res2]) => Service)
  extends NamedServiceDescription[M, Req, Res, Service] {

  override lazy val service: (Req) => M[Res] = maker(service1.service, service2.service)

  override def shortToString: String = s"${registryId}:Merging[${implicitly[ClassTag[Req]].runtimeClass.getSimpleName},${implicitly[ClassTag[Res]].runtimeClass.getSimpleName}]"

  override def children: List[AbstractServiceDescription[M, _, _]] = List(service1, service2)
}

trait MakeServiceDescription[M[_], OldReq, OldRes, Req, Res] {
  def apply(delegate: AbstractServiceDescription[M, OldReq, OldRes])(implicit classTagOldReq: ClassTag[OldReq], classTagOldRes: ClassTag[OldRes], classTagReq: ClassTag[Req], classTagRes: ClassTag[Res]): AbstractServiceDescription[M, Req, Res]
}
