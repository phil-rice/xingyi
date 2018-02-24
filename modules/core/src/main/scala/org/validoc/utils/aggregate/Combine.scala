package org.validoc.utils.aggregate

import org.validoc.utils.containers.Monad
import org.validoc.utils._
import scala.language.higherKinds

class Combine[M[_]] {
  def apply[Req, Res1, Res2](k1: Req => M[Res1], k2: Req => M[Res2]) = new Combine2[M, Req, Res1, Res2](k1, k2)

  def apply[Req, Res1, Res2, Res3](k1: Req => M[Res1], k2: Req => M[Res2], k3: Req => M[Res3]) = new Combine3[M, Req, Res1, Res2, Res3](k1, k2, k3)

  def apply[Req, Res1, Res2, Res3, Res4](k1: Req => M[Res1], k2: Req => M[Res2], k3: Req => M[Res3], k4: Req => M[Res4]) = new Combine4[M, Req, Res1, Res2, Res3, Res4](k1, k2, k3, k4)
}


class Combine2[M[_], Req, Res1, Res2](k1: Req => M[Res1], k2: Req => M[Res2]) {
  def mergeInto[Res](merger: (Req, Res1, Res2) => Res)(implicit asyncMonad: Monad[M]): (Req => M[Res]) = { req =>
    val m1 = k1(req)
    val m2 = k2(req)
    m1.flatMap(r1 => m2.map(r2 => merger(req, r1, r2)))
  }
}

class Combine3[M[_], Req, Res1, Res2, Res3](k1: Req => M[Res1], k2: Req => M[Res2], k3: Req => M[Res3]) {
  def mergeInto[Res](merger: (Req, Res1, Res2, Res3) => Res)(implicit asyncMonad: Monad[M]): (Req => M[Res]) = { req =>
    val m1 = k1(req)
    val m2 = k2(req)
    val m3 = k3(req)
    m1.flatMap(r1 => m2.flatMap(r2 => m3.map(r3 => merger(req, r1, r2, r3))))
  }
}

class Combine4[M[_], Req, Res1, Res2, Res3, Res4](k1: Req => M[Res1], k2: Req => M[Res2], k3: Req => M[Res3], k4: Req => M[Res4]) {
  def mergeInto[Res](merger: (Req, Res1, Res2, Res3, Res4) => Res)(implicit asyncMonad: Monad[M]): (Req => M[Res]) = { req =>
    val m1 = k1(req)
    val m2 = k2(req)
    val m3 = k3(req)
    val m4 = k4(req)
    m1.flatMap(r1 => m2.flatMap(r2 => m3.flatMap(r3 => m4.map(r4 => merger(req, r1, r2, r3, r4)))))
  }
}

