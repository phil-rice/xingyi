package org.validoc.utils.gash

import scala.language.higherKinds

trait Merger[T1, T2, R] extends ((T1, T2) => R)

trait JoinAsync[M[_]] {
  def join[T1, T2](a: M[T1], b: M[T2]): M[(T1, T2)]
}

trait Mapable[M[_]] {
  def map[T1, T2](t1: M[T1], fn: T1 => T2): M[T2]
}

trait FindId[Main, Id] extends (Main => Id)

trait Arrows {

  implicit class FunctionPimper[T1, T2](fn: T1 => T2) {
    def ->[T3](fn2: T2 => T3) = fn andThen fn2
  }

  implicit class FunctionMPimper[M[_], T1, T2](fn: T1 => M[T2])(implicit mapable: Mapable[M]) {
    def map[T3](fn2: T2 => T3) = { t1: T1 => mapable.map(fn(t1), fn2) }

    def ->[T3](fn2: T2 => T3) = map(fn2)

    def join[T3, T4](fn2: T3 => M[T4])(implicit joinAsync: JoinAsync[M]): (T1, T3) => M[(T2, T4)] =
      (t1, t2) => joinAsync.join[T2, T4](fn(t1), fn2(t2))

  }

  implicit class MergePimper[M[_], T1, T2, T3, T4](tupleFn: (T1, T2) => M[(T3, T4)]) {
    def merge[Req, Res](implicit merger: Merger[T3, T4, Res], mapable: Mapable[M]): (T1, T2) => M[Res] =
      (t1, t2) => mapable.map[(T3, T4), Res](tupleFn(t1, t2), t => merger(t._1, t._2))
  }

  def splitAndMerge[M[_], Req, Res1, Res2, Res](
                                                 path1: Req => M[Res1],
                                                 path2: Req => M[Res2])
                                               (implicit merger: Merger[Res1, Res2, Res],
                                                joinAsync: JoinAsync[M],
                                                mapable: Mapable[M]): Req => M[Res] = req =>
    mapable.map[(Res1, Res2), Res](joinAsync.join(path1(req), path2(req)), t => merger(t._1, t._2))


}

class MergingService[M[_], ReqO, ResO, Req1, Res1, Req2, Res2](service1: Req1 => M[Res1], service2: Req2 => M[Res2])
                                                              (implicit merger: Merger[Res1, Res2, ResO],
                                                               id1: FindId[ReqO, Req1],
                                                               id2: FindId[ReqO, Req2],
                                                               joinAsync: JoinAsync[M],
                                                               mapable: Mapable[M]) extends (ReqO => M[ResO]) with Arrows {
  override def apply(req: ReqO): M[ResO] = splitAndMerge(id1 -> service1, id2 -> service2) apply (req)

}

