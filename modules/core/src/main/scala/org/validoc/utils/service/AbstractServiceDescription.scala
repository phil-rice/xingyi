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

  class TypeProblemBreader[T](serviceReporter: ServiceReporter[T]) extends (Any => Option[String]) {
    def apply(t: Any) = serviceReporter(t.asInstanceOf[T])

    override def toString(): String = s"TypeProblemBreaderxx(${serviceReporter})"
  }

  def typeProblemBreaker[T](serviceReporter: ServiceReporter[T]): Any => Option[String] = new TypeProblemBreader[T](serviceReporter)

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

abstract class AbstractServiceDescription[M[_], Req: ClassTag, Res: ClassTag](val serviceReporter: Any => Option[String]) extends ServiceDescription[M] { //I had to make the service report Any => String here. I would like it more strong typed but I couldn't make that work
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

case class RootServiceDescription[M[_], Param, HttpReq: ClassTag, HttpRes: ClassTag](param: Param, makeHttpService: Param => HttpReq => M[HttpRes])
                                                                                    (implicit serviceReporter: ServiceReporter[HttpReq => M[HttpRes]])
  extends AbstractServiceDescription[M, HttpReq, HttpRes](ServiceReporter.typeProblemBreaker(serviceReporter)) {

  lazy val service = makeHttpService(param)

  override def shortToString = s"${registryId}:RootHttp($param)"

  override def children: List[AbstractServiceDescription[M, _, _]] = List()
}

abstract class NamedServiceDescription[M[_], Req: ClassTag, Res: ClassTag, Service <: Req => M[Res] : ClassTag](implicit serviceReporter: ServiceReporter[Service])
  extends AbstractServiceDescription[M, Req, Res](ServiceReporter.typeProblemBreaker(serviceReporter)) {

  import org.validoc.utils.reflection.ClassTags._

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
