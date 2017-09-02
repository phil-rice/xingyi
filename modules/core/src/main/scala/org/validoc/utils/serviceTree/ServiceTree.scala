package org.validoc.utils.serviceTree

import org.validoc.utils.aggregate.AggregateServiceLanguageExtension
import org.validoc.utils.caching.CacheServiceLanguage
import org.validoc.utils.concurrency.Async
import org.validoc.utils.endpoint.{DebugEndPointServiceLanguageExtension, EndPointServiceLanguageExtension}
import org.validoc.utils.http.{HttpObjectServiceLanguageExtension, HttpServiceLanguageExtension}
import org.validoc.utils.logging.LoggingClientServiceLanguageExtension
import org.validoc.utils.profiling.ProfilingServiceLanguageExtension

import scala.reflect.ClassTag
import scala.language.higherKinds


object ServiceTree {

  implicit class ServiceTreeHelper[M[_], Req, Res, Payload](tree: ServiceTree[M, Req, Res, Payload]) {
    def map[NewPayload](fn: Payload => NewPayload): ServiceTree[M, Req, Res, NewPayload] = tree.mapFromTree(x => fn(x.payload))

    def foldToList = tree.foldFromTree(List[Payload]())(_ :+ _.payload)

    def foldToListOfTrees = tree.foldFromTree(List[ServiceTree[M, _, _, Payload]]())(_ :+ _)

    type IndentTreeAcc = (List[(ServiceTree[M, _, _, Payload], Int)], Int)

    def foldToListOfTreesAndDepth: List[(ServiceTree[M, _, _, Payload], Int)] = {
      val resultWithWrongDepths = tree.foldFromTree[IndentTreeAcc]((List(), 0)) { case ((list, depth), tree) => ((tree, depth) :: list, depth + 1) }._1
      val maxDepth = resultWithWrongDepths.map(_._2).max
      resultWithWrongDepths.map { case (tree, depth) => (tree, maxDepth - depth) }
    }

    def filter(acceptor: ServiceTree[M, _, _, Payload] => Boolean) = foldToListOfTrees.filter(acceptor)

    def collect[X](acceptor: PartialFunction[ServiceTree[M, _, _, Payload], X]) = foldToListOfTrees.collect(acceptor)

    def findAll[X](implicit classTag: ClassTag[X]) = filter(x => classTag.runtimeClass.isAssignableFrom(x.service.getClass))
    def findAllServices[X](implicit classTag: ClassTag[X]) = filter(x => classTag.runtimeClass.isAssignableFrom(x.service.getClass)).map(_.service.asInstanceOf[X])

    def findAllWithReqRes[NewReq, NewRes](implicit reqTag: ClassTag[NewReq], resTag: ClassTag[NewRes]) = filter(st => st.reqClassTag == reqTag && st.resClassTag == resTag).map(_.asInstanceOf[ServiceTree[M, NewReq, NewRes, Payload]])

    def findAllTreesWithServiceReqRes[NewReq, NewRes, Service <: NewReq => M[NewRes]](implicit reqTag: ClassTag[NewReq], resTag: ClassTag[NewRes], serviceClassTag: ClassTag[Service]) =
      filter(st => st.reqClassTag == reqTag && st.resClassTag == resTag && serviceClassTag.runtimeClass.isAssignableFrom(st.service.getClass)).map(_.asInstanceOf[ServiceTree[M, NewReq, NewRes, Payload]])

    def allHttpServices[HttpReq: ClassTag, HttpRes: ClassTag] = findAllWithReqRes[HttpReq, HttpRes].collect { case st: RootServiceTree[M, HttpReq, HttpRes, Payload] => st }

  }

}


sealed abstract class ServiceTree[M[_] : Async, Req, Res, Payload](implicit val reqClassTag: ClassTag[Req], val resClassTag: ClassTag[Res]) {
  def payload: Payload

  /** Important constraints: Keep the same service even after map, and on multiple calls. This is the 'real' service */
  def service: Req => M[Res]


  def mapFromTree[NewPayload](fn: ServiceTree[M, _, _, Payload] => NewPayload): ServiceTree[M, Req, Res, NewPayload]

  def foldFromTree[Acc](initial: Acc)(foldFn: (Acc, ServiceTree[M, _, _, Payload]) => Acc): Acc

}

case class RootServiceTree[M[_] : Async, Req: ClassTag, Res: ClassTag, Payload](payload: Payload, serviceMaker: () => Req => M[Res]) extends ServiceTree[M, Req, Res, Payload] {
  override lazy val service: (Req) => M[Res] = serviceMaker()

  override def mapFromTree[NewPayload](fn: (ServiceTree[M, _, _, Payload]) => NewPayload) = RootServiceTree(fn(this), () => service)

  override def foldFromTree[Acc](initial: Acc)(foldFn: (Acc, ServiceTree[M, _, _, Payload]) => Acc): Acc = foldFn(initial, this)

}

case class DelegateTree0[M[_] : Async, Req: ClassTag, Res: ClassTag, Payload](delegate: ServiceTree[M, Req, Res, Payload], payload: Payload, serviceMaker: (Req => M[Res]) => Req => M[Res]) extends ServiceTree[M, Req, Res, Payload] {
  override lazy val service: (Req) => M[Res] = serviceMaker(delegate.service)

  override def mapFromTree[NewPayload](fn: (ServiceTree[M, _, _, Payload]) => NewPayload) = DelegateTree0(delegate.mapFromTree(fn), fn(this), _ => service)

  override def foldFromTree[Acc](initial: Acc)(foldFn: (Acc, ServiceTree[M, _, _, Payload]) => Acc): Acc = foldFn(delegate.foldFromTree(initial)(foldFn), this)

}


case class TransformerTree0[M[_] : Async, OldReq, OldRes, NewReq: ClassTag, NewRes: ClassTag, Payload](delegate: ServiceTree[M, OldReq, OldRes, Payload], payload: Payload, serviceMaker: (OldReq => M[OldRes]) => NewReq => M[NewRes]) extends ServiceTree[M, NewReq, NewRes, Payload] {
  override lazy val service: (NewReq) => M[NewRes] = serviceMaker(delegate.service)

  override def mapFromTree[NewPayload](fn: (ServiceTree[M, _, _, Payload]) => NewPayload): ServiceTree[M, NewReq, NewRes, NewPayload] = TransformerTree0(delegate.mapFromTree(fn), fn(this), { someParam: (OldReq => M[OldRes]) => service })

  override def foldFromTree[Acc](initial: Acc)(foldFn: (Acc, ServiceTree[M, _, _, Payload]) => Acc): Acc = foldFn(delegate.foldFromTree(initial)(foldFn), this)

}

case class TwoChildrenTree[M[_] : Async, ReqFull: ClassTag, ResFull: ClassTag, Child1Req, Child1Res, Child2Req, Child2Res, Payload](
                                                                                                                                     delegateTree1: ServiceTree[M, Child1Req, Child1Res, Payload],
                                                                                                                                     delegateTree2: ServiceTree[M, Child2Req, Child2Res, Payload],
                                                                                                                                     payload: Payload,
                                                                                                                                     serviceMaker: (Child1Req => M[Child1Res], Child2Req => M[Child2Res]) => ReqFull => M[ResFull]
                                                                                                                                   ) extends ServiceTree[M, ReqFull, ResFull, Payload] {

  /** Important constraints: Keep the same service even after map, and on multiple calls. This is the 'real' service */
  override lazy val service = serviceMaker(delegateTree1.service, delegateTree2.service)

  override def mapFromTree[NewPayload](fn: (ServiceTree[M, _, _, Payload]) => NewPayload) =
    TwoChildrenTree[M, ReqFull, ResFull, Child1Req, Child1Res, Child2Req, Child2Res, NewPayload](delegateTree1.mapFromTree(fn), delegateTree2.mapFromTree(fn), fn(this), (_, _) => service)

  override def foldFromTree[Acc](initial: Acc)(foldFn: (Acc, ServiceTree[M, _, _, Payload]) => Acc) = {
    val first = delegateTree1.foldFromTree(initial)(foldFn)
    val second = delegateTree2.foldFromTree(first)(foldFn)
    foldFn(second, this)
  }
}


trait ServiceLanguageExtension[M[_]] {
  protected implicit def async: Async[M]

  type ServiceTransformer[OldReq, OldRes, Req, Res] =
    ServiceTree[M, OldReq, OldRes, ServiceDescription] =>
      ServiceTree[M, Req, Res, ServiceDescription]
  type ServiceDelegator[Req, Res] = ServiceTransformer[Req, Res, Req, Res]
  type ServiceAggregator[ReqFull, ResFull, Child1Req, Child1Res, Child2Req, Child2Res] =
    (ServiceTree[M, Child1Req, Child1Res, ServiceDescription], ServiceTree[M, Child2Req, Child2Res, ServiceDescription]) => ServiceTree[M, ReqFull, ResFull, ServiceDescription]


  //    => ServiceCreator[Req, Res, NewService, Payload])

  implicit class ServiceDescriptionPimper[OldReq, OldRes](sd: ServiceTree[M, OldReq, OldRes, ServiceDescription]) {
    def >--<[NewReq, NewRes](transformer: ServiceTransformer[OldReq, OldRes, NewReq, NewRes]): ServiceTree[M, NewReq, NewRes, ServiceDescription] =
      transformer(sd)

  }

  def root[Req: ClassTag, Res: ClassTag](description: String, serviceMaker: () => Req => M[Res]) =
    RootServiceTree[M, Req, Res, ServiceDescription](ServiceDescription(description), serviceMaker)

  def delegate[Req: ClassTag, Res: ClassTag](description: String, delegate: ServiceTree[M, Req, Res, ServiceDescription], serviceMaker: (Req => M[Res]) => Req => M[Res]) =
    DelegateTree0(delegate, ServiceDescription(description), serviceMaker)

  def transform[OldReq: ClassTag, OldRes: ClassTag, NewReq: ClassTag, NewRes: ClassTag](description: String, delegate: ServiceTree[M, OldReq, OldRes, ServiceDescription], serviceMaker: (OldReq => M[OldRes]) => NewReq => M[NewRes]) =
    TransformerTree0(delegate, ServiceDescription(description), serviceMaker)

  def twoServices[ReqFull: ClassTag, ResFull: ClassTag, Child1Req, Child1Res, Child2Req, Child2Res, Payload](
                                                                                                              description: String,
                                                                                                              child1: ServiceTree[M, Child1Req, Child1Res, ServiceDescription],
                                                                                                              child2: ServiceTree[M, Child2Req, Child2Res, ServiceDescription],
                                                                                                              serviceMaker: (Child1Req => M[Child1Res], Child2Req => M[Child2Res]) => ReqFull => M[ResFull]
                                                                                                            ) = {
    TwoChildrenTree(child1, child2, ServiceDescription(description), serviceMaker)
  }

}

case class ServiceDescription(description: String)

//
//trait HttpServiceLanguageExtension extends ServiceLanguageExtension {
//  def http(hostNameAndPort: String) = root(s"FinagleHttp($hostNameAndPort)", () => Http.newService(hostNameAndPort))
//}

trait HttpReqHttpResServiceLanguageExtension[M[_], HttpReq, HttpRes] extends ServiceLanguage[M] with HttpObjectServiceLanguageExtension[M, HttpReq, HttpRes] with HttpServiceLanguageExtension[M, HttpReq, HttpRes]


trait ServiceLanguage[M[_]] extends LoggingClientServiceLanguageExtension[M]
  with CacheServiceLanguage[M]
  with DebugEndPointServiceLanguageExtension[M]
  with EndPointServiceLanguageExtension[M]
  with ProfilingServiceLanguageExtension[M]
with AggregateServiceLanguageExtension[M]

