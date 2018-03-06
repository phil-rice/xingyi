package org.validoc.utils.aggregate

import org.validoc.utils.functions.Monad
import org.validoc.utils.language.Language.{join2WithReq, join3WithReq, join4WithReq}
import org.validoc.utils.language.Language._

trait MergeKleisli[M[_]] {
  protected implicit def monad: Monad[M]
 private  type Kleisli[Req, Res] = (Req => M[Res])
  def merge2Prim[ReqM, ResM, Req1, Res1, Req2, Res2](firstService: Kleisli[Req1, Res1], secondService: Kleisli[Req2, Res2], merger: (ReqM, Res1, Res2) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2): ReqM => M[ResM] =
    join2WithReq(
      reqMtoReq1 ~> firstService,
      reqMtoReq2 ~> secondService
    ) |=> merger.tupled

  def merge3Prim[ReqM, ResM, Req1, Res1, Req2, Res2, Req3, Res3](firstService: Kleisli[Req1, Res1], secondService: Kleisli[Req2, Res2], thirdService: Kleisli[Req3, Res3], merger: (ReqM, Res1, Res2, Res3) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3): ReqM => M[ResM] =
    join3WithReq(
      reqMtoReq1 ~> firstService,
      reqMtoReq2 ~> secondService,
      reqMtoReq3 ~> thirdService,
    ) |=> merger.tupled

  def merge4Prim[ReqM, ResM, Req1, Res1, Req2, Res2, Req3, Res3, Req4, Res4](firstService: Kleisli[Req1, Res1], secondService: Kleisli[Req2, Res2], thirdService: Kleisli[Req3, Res3], fourthService: Kleisli[Req4, Res4], merger: (ReqM, Res1, Res2, Res3, Res4) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3, reqMtoReq4: ReqM => Req4): ReqM => M[ResM] =
    join4WithReq(
      reqMtoReq1 ~> firstService,
      reqMtoReq2 ~> secondService,
      reqMtoReq3 ~> thirdService,
      reqMtoReq4 ~> fourthService,
    ) |=> merger.tupled
}
