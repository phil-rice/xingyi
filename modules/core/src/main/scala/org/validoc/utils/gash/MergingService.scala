package org.validoc.utils.gash

import org.validoc.utils.concurrency.Async
import org.validoc.utils.service.MakeServiceMakerForTwoServices

import scala.language.higherKinds

trait Merger[T1, T2, R] extends ((T1, T2) => R)


trait FindId[Main, Id] extends (Main => Id)

class MergingService[M[_] : Async, ReqO, ResO, Req1, Res1, Req2, Res2](service1: Req1 => M[Res1], service2: Req2 => M[Res2])
                                                                      (implicit merger: Merger[Res1, Res2, ResO],
                                                                       id1: FindId[ReqO, Req1],
                                                                       id2: FindId[ReqO, Req2]) extends (ReqO => M[ResO]) {

  import Async._

  override def apply(req: ReqO): M[ResO] = {
    implicitly[Async[M]].join2(service1(id1(req)), service2(id2(req))).map[ResO](merger.tupled)
  }

}

object MergingService {

  implicit def mergingServiceMaker[M[_]:Async, ReqO, ResO, Req1, Res1, Req2, Res2](implicit merger: Merger[Res1, Res2, ResO],
                                                                             id1: FindId[ReqO, Req1],
                                                                             id2: FindId[ReqO, Req2]) = new MakeServiceMakerForTwoServices[Req1 => M[Res1], Req2 => M[Res2], MergingService[M, ReqO, ResO, Req1, Res1, Req2, Res2]] {
    override def apply(old1: (Req1) => M[Res1], old2: (Req2) => M[Res2]): MergingService[M, ReqO, ResO, Req1, Res1, Req2, Res2] =
      new MergingService[M, ReqO, ResO, Req1, Res1, Req2, Res2](old1, old2)
  }
}
