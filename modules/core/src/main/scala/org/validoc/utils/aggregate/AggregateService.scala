package org.validoc.utils.aggregate

import org.validoc.utils.Service
import org.validoc.utils.concurrency.Async
import org.validoc.utils.concurrency.Async._
import org.validoc.utils.service.MakeServiceMakerForTwoServices

import scala.language.higherKinds

trait HasChildren[Parent, Child] {
  def apply(p: Parent): Seq[Child]
}

trait Enricher[Enriched, Parent, Child] {
  def apply(p: Parent)(children: Seq[Child]): Enriched
}


class EnrichParentChildService[M[_] : Async, ReqP, ResP, ReqC, ResC, ResE](parentService: Service[M, ReqP, ResP],
                                                                           childService: Service[M, ReqC, ResC])
                                                                          (implicit children: HasChildren[ResP, ReqC],
                                                                           enricher: Enricher[ResE, ResP, ResC]) extends Service[M, ReqP, ResE] {
  override def apply(reqP: ReqP): M[ResE] = parentService(reqP).flatMap(resP => children(resP).map(childService).join.map(enricher(resP)))

}

object EnrichParentChildService {
  implicit def makeEnrichParentChildService[M[_] : Async, ReqP, ResP, ReqC, ResC, ResE](implicit
                                                                                        children: HasChildren[ResP, ReqC],
                                                                                        enricher: Enricher[ResE, ResP, ResC]) =
    new MakeServiceMakerForTwoServices[ReqP => M[ResP], ReqC => M[ResC], EnrichParentChildService[M, ReqP, ResP, ReqC, ResC, ResE]] {
      override def apply(old1: (ReqP) => M[ResP], old2: (ReqC) => M[ResC]): EnrichParentChildService[M, ReqP, ResP, ReqC, ResC, ResE] =
        new EnrichParentChildService[M, ReqP, ResP, ReqC, ResC, ResE](old1, old2)
    }

//  EnrichParentChildService[M, Req1, Res1, Req2, Res2, ResE]]
}

class MergeService[M[_] : Async, ReqM, ResM, Req1, Res1, Req2, Res2](firstService: Service[M, Req1, Res1],
                                                                     secondService: Service[M, Req2, Res2],
                                                                     merger: (Res1, Res2) => ResM)
                                                                    (implicit reqMtoReq1: ReqM => Req1,
                                                                     reqMtoReq2: ReqM => Req2)
  extends Service[M, ReqM, ResM] {
  override def apply(req: ReqM): M[ResM] = {
    firstService(req) join secondService(req) map { case (r1, r2) => merger(r1, r2) }
  }
}
