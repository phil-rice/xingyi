package org.validoc.utils.aggregate

import scala.language.higherKinds

class SplitWord[M[_], Req] {
  def apply[Req1, Res1, Req2, Res2](k1: Req1 => M[Res1], k2: Req2 => M[Res2])(implicit findReq1: FindReq[Req, Req1], findReq2: FindReq[Req, Req2]): Combine2[M, Req, Res1, Res2] = {
    new Combine2[M, Req, Res1, Res2](findReq1 andThen k1, findReq2 andThen k2)
  }
}


class Split[M[_]] {
  def splitFrom[Req] = new SplitWord[M, Req]
}
