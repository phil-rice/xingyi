package one.xingyi.core.aggregate
import one.xingyi.core.language.Language.{join2WithReq, join3WithReq, join4WithReq, _}
import one.xingyi.core.monad.Monad

import scala.language.higherKinds
import scala.reflect.ClassTag

trait FindReq[Main, Req] extends (Main => Req)

object FindReq {
  implicit def identityFindReq[X] = new FindReq[X, X] {
    override def apply(v1: X): X = v1
  }
}

trait MergeLanguage[Wrapper[_, _]] {
  def merge2Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2](firstService: Wrapper[Req1, Res1], secondService: Wrapper[Req2, Res2], merger: (ReqM, Res1, Res2) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2): Wrapper[ReqM, ResM]

  def merge3Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2, Req3, Res3](firstService: Wrapper[Req1, Res1], secondService: Wrapper[Req2, Res2], thirdService: Wrapper[Req3, Res3], merger: (ReqM, Res1, Res2, Res3) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3): Wrapper[ReqM, ResM]

  def merge4Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2, Req3, Res3, Req4, Res4](firstService: Wrapper[Req1, Res1], secondService: Wrapper[Req2, Res2], thirdService: Wrapper[Req3, Res3], fourthService: Wrapper[Req4, Res4], merger: (ReqM, Res1, Res2, Res3, Res4) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3, reqMtoReq4: ReqM => Req4): Wrapper[ReqM, ResM]


  protected class Merge[Req1, Res1](firstService: Wrapper[Req1, Res1]) {
    case class and[Req2, Res2](secondService: Wrapper[Req2, Res2]) {
      def into[ReqM: ClassTag, ResM: ClassTag](merger: (ReqM, Res1, Res2) => ResM)(implicit findReq1: FindReq[ReqM, Req1], findReq2: FindReq[ReqM, Req2]): Wrapper[ReqM, ResM] =
        merge2Prim(firstService, secondService, merger)

      case class and[Req3, Res3](thirdService: Wrapper[Req3, Res3]) {
        def into[ReqM: ClassTag, ResM: ClassTag](merger: (ReqM, Res1, Res2, Res3) => ResM)(implicit findReq1: FindReq[ReqM, Req1], findReq2: FindReq[ReqM, Req2], findReq3: FindReq[ReqM, Req3]): Wrapper[ReqM, ResM] =
          merge3Prim(firstService, secondService, thirdService, merger)

        case class and[Req4, Res4](fourthService: Wrapper[Req4, Res4]) {
          def into[ReqM: ClassTag, ResM: ClassTag](merger: (ReqM, Res1, Res2, Res3, Res4) => ResM)(implicit findReq1: FindReq[ReqM, Req1], findReq2: FindReq[ReqM, Req2], findReq3: FindReq[ReqM, Req3], findReq4: FindReq[ReqM, Req4]): Wrapper[ReqM, ResM] = merge4Prim(firstService, secondService, thirdService, fourthService, merger)
        }
      }
    }
  }

  def merge[Req1, Res1](firstService: Wrapper[Req1, Res1]) = new Merge[Req1, Res1](firstService)


}

trait MergeKleisli[M[_]] {
  protected implicit def monad: Monad[M]
  private type Kleisli[Req, Res] = (Req => M[Res])
  def merge2Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2](firstService: Kleisli[Req1, Res1], secondService: Kleisli[Req2, Res2], merger: (ReqM, Res1, Res2) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2): ReqM => M[ResM] =
    join2WithReq(
      reqMtoReq1 ~> firstService,
      reqMtoReq2 ~> secondService
    ) |=> merger.tupled

  def merge3Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2, Req3, Res3](firstService: Kleisli[Req1, Res1], secondService: Kleisli[Req2, Res2], thirdService: Kleisli[Req3, Res3], merger: (ReqM, Res1, Res2, Res3) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3): ReqM => M[ResM] =
    join3WithReq(
      reqMtoReq1 ~> firstService,
      reqMtoReq2 ~> secondService,
      reqMtoReq3 ~> thirdService,
    ) |=> merger.tupled

  def merge4Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2, Req3, Res3, Req4, Res4](firstService: Kleisli[Req1, Res1], secondService: Kleisli[Req2, Res2], thirdService: Kleisli[Req3, Res3], fourthService: Kleisli[Req4, Res4], merger: (ReqM, Res1, Res2, Res3, Res4) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3, reqMtoReq4: ReqM => Req4): ReqM => M[ResM] =
    join4WithReq(
      reqMtoReq1 ~> firstService,
      reqMtoReq2 ~> secondService,
      reqMtoReq3 ~> thirdService,
      reqMtoReq4 ~> fourthService,
    ) |=> merger.tupled
}
