package org.validoc.utils.aggregate

import org.validoc.utils.Service
import org.validoc.utils.arrows.Arrows._
import org.validoc.utils.concurrency.Async
import org.validoc.utils.serviceTree.{ServiceDescription, ServiceLanguageExtension, ServiceTree}

import scala.language.higherKinds
import scala.reflect.ClassTag

trait HasChildren[Parent, Child] extends (Parent => Seq[Child])

trait Enricher[Enriched, Parent, Child] extends ((Parent, Seq[Child]) => Enriched)


class EnrichParentChildService[M[_] : Async, ReqP, ResP, ReqC, ResC, ResE](parentService: Service[M, ReqP, ResP],
                                                                           childService: Service[M, ReqC, ResC])
                                                                          (implicit findChildIds: HasChildren[ResP, ReqC],
                                                                           enricher: Enricher[ResE, ResP, ResC]) extends Service[M, ReqP, ResE] {
  override def apply(reqP: ReqP): M[ResE] = (parentService.tee(findChildIds ~~> childService) ~~> enricher.tupled) (reqP)
}


class MergeService[M[_] : Async, ReqM, ResM, Req1, Res1, Req2, Res2](firstService: Service[M, Req1, Res1],
                                                                     secondService: Service[M, Req2, Res2],
                                                                     merger: (Res1, Res2) => ResM)
                                                                    (implicit reqMtoReq1: ReqM => Req1,
                                                                     reqMtoReq2: ReqM => Req2)
  extends Service[M, ReqM, ResM] {
  override def apply(req: ReqM): M[ResM] = (reqMtoReq1 ~> firstService, reqMtoReq2 ~> secondService).join(merger)(req)
}

trait AggregateTransform[From, To] extends (From => To)

object AggregateTransform {
  implicit def transfromToSelf[T] = new AggregateTransform[T, T] {
    override def apply(v1: T) = v1
  }
}

trait AggregateMerger[Child1, Child2, Merged] extends (((Child1, Child2)) => Merged)

class AggregateService[M[_], ReqFull, ResFull, Child1Req, Child1Res, Child2Req, Child2Res]
(child1: Child1Req => M[Child1Res], child2: Child2Req => M[Child2Res])
(implicit async: Async[M], reqToChild1Req: AggregateTransform[ReqFull, Child1Req],
 reqToChild2Req: AggregateTransform[ReqFull, Child2Req],
 merger: AggregateMerger[Child1Res, Child2Res, ResFull]) extends (ReqFull => M[ResFull]) {
  override def apply(req: ReqFull) = {
    new Async.AsyncPimper(async.join2(child1(reqToChild1Req(req)), child2(reqToChild2Req(req)))).map[ResFull](merger)
  }
}

trait AggregateServiceLanguageExtension[M[_]] extends ServiceLanguageExtension[M] {

  //  def aggregate[ReqFull: ClassTag, ResFull: ClassTag, Child1Req: ClassTag, Child1Res: ClassTag, Child2Req: ClassTag, Child2Res: ClassTag](description: String)(implicit
  //                                                                                                                                                               reqToChild1Req: AggregateTransform[ReqFull, Child1Req],
  //                                                                                                                                                               reqToChild2Req: AggregateTransform[ReqFull, Child2Req],
  //                                                                                                                                                               merger: AggregateMerger[Child1Res, Child2Res, ResFull]):
  //  ServiceAggregator[ReqFull, ResFull, Child1Req, Child1Res, Child2Req, Child2Res] = {
  //    (childTree1, childTree2) =>
  //      twoServices[ReqFull, ResFull, Child1Req, Child1Res, Child2Req, Child2Res, ServiceDescription](description, childTree1, childTree2, (t1, t2) => new AggregateService(t1, t2))
  //  }

  implicit class AggregatedTuplePimper[Child1Req: ClassTag, Child1Res: ClassTag, Child2Req: ClassTag, Child2Res: ClassTag]
  (tuple: (ServiceTree[M, Child1Req, Child1Res, ServiceDescription], ServiceTree[M, Child2Req, Child2Res, ServiceDescription])) {
    def agg[ReqFull: ClassTag, ResFull: ClassTag](description: String)(implicit
                                                                       reqToChild1Req: AggregateTransform[ReqFull, Child1Req],
                                                                       reqToChild2Req: AggregateTransform[ReqFull, Child2Req],
                                                                       merger: AggregateMerger[Child1Res, Child2Res, ResFull]) =
      twoServices[ReqFull, ResFull, Child1Req, Child1Res, Child2Req, Child2Res, ServiceDescription](description, tuple._1, tuple._2, (service1, service2) => new AggregateService[M, ReqFull, ResFull, Child1Req, Child1Res, Child2Req, Child2Res](service1, service2))

    def merge[ReqFull: ClassTag, ResFull: ClassTag](implicit reqMtoReq1: AggregateTransform[ReqFull, Child1Req],
                                                    reqMtoReq2: AggregateTransform[ReqFull, Child2Req],
                                                    merger: (Child1Res, Child2Res) => ResFull) = {
      twoServices[ReqFull, ResFull, Child1Req, Child1Res, Child2Req, Child2Res, ServiceDescription](s"Merge", tuple._1, tuple._2,
        (service1, service2) => new MergeService[M, ReqFull, ResFull, Child1Req, Child1Res, Child2Req, Child2Res](service1, service2, merger))
    }

    def enrich[ResE: ClassTag](implicit findChildIds: HasChildren[Child1Res, Child2Req],
                               enricher: Enricher[ResE, Child1Res, Child2Res]) = twoServices[Child1Req, ResE, Child1Req, Child1Res, Child2Req, Child2Res, ServiceDescription](s"Enriching", tuple._1, tuple._2,
      (service1, service2) => new EnrichParentChildService[M, Child1Req, Child1Res, Child2Req, Child2Res, ResE](service1, service2))
  }

}