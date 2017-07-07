package org.validoc.utils.aggregate

import org.validoc.utils.Service
import org.validoc.utils.concurrency.Async
import org.validoc.utils.concurrency.Async._
import org.validoc.utils.arrows.Arrows._

import scala.language.higherKinds

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
