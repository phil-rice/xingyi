package org.validoc.utils.gash

import org.validoc.utils.containers.Monad

import scala.language.higherKinds
import org.validoc.utils._

trait Merger[T1, T2, R] extends ((T1, T2) => R)


trait FindId[Main, Id] extends (Main => Id)

class MergingService[M[_] : Monad, ReqO, ResO, Req1, Res1, Req2, Res2](service1: Req1 => M[Res1], service2: Req2 => M[Res2])
                                                                      (implicit merger: Merger[Res1, Res2, ResO],
                                                                       id1: FindId[ReqO, Req1],
                                                                       id2: FindId[ReqO, Req2]) extends (ReqO => M[ResO]) {

  override def apply(req: ReqO): M[ResO] = {
    implicitly[Monad[M]].join2(service1(id1(req)), service2(id2(req))).map[ResO](merger.tupled)
  }

}
