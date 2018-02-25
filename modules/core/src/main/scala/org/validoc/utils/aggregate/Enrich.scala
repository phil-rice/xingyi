package org.validoc.utils.aggregate

import org.validoc.utils._
import org.validoc.utils.functions.{Functor, Monad}

import scala.language.higherKinds
trait FindReq[Main,Req] extends (Main => Req)

class EnrichWord[M[_] : Monad, Req, Res](enrich: Enrich[M], delegate: Req => M[Res]) {

  def onceWith[ReqC, ResC](childK: ReqC => M[ResC])(implicit findReq: FindReq[Res, ReqC]) = new SingleEnrichDetailsWord[M, Req, Res, ReqC, ResC](enrich,delegate, childK)
  def seqWith[ReqC, ResC](childK: ReqC => M[ResC])(implicit findReq: FindReq[Res, Seq[ReqC]]) = new MultipleEnrichDetailsWord[M, Req, Res, ReqC, ResC](enrich,delegate, childK)
}

class Remember[M[_] : Functor] {
  def remember[Req, Res](k: Req => M[Res]) = new RememberWord1[M, Req, Res](k)
}

class Enrich[M[_] : Monad] extends Remember[M] {
  def apply[Req1, Res1](k: Req1 => M[Res1]) = new EnrichWord[M, Req1, Res1](this, k)
}

class RememberWord1[M[_] , Req, Res1](k1: Req => M[Res1]) {
  def andThen[Res2](k2: Res1 => M[Res2])(implicit async: Monad[M]) = new RememberWord2[M, Req, Res1, Res2](k1, k2)
  def finishWith[Res](fn: (Req, Res1) => Res)(implicit functor: Functor[M]): (Req => M[Res]) = { req => k1(req).map(res => fn(req, res)) }
}

class RememberWord2[M[_] : Monad, Req, Res1, Res2](k1: Req => M[Res1], k2: Res1 => M[Res2]) {
  def finishWith[Res](fn: (Req, Res1, Res2) => Res): (Req => M[Res]) = { req => k1(req).flatMap(res1 => k2(res1).map(res2 => fn(req, res1, res2))) }
}

class SingleEnrichDetailsWord[M[_] : Monad, Req1, Res1, ReqC, ResC](enrich: Enrich[M],first: Req1 => M[Res1], child: ReqC => M[ResC])(implicit findReq: FindReq[Res1, ReqC]) {
  def into[Res](fn: (Req1, Res1, ResC) => Res): Req1 => M[Res] = enrich.remember(first) andThen (findReq andThen child) finishWith fn
}

class MultipleEnrichDetailsWord[M[_] : Monad, Req1, Res1, ReqC, ResC](enrich: Enrich[M],first: Req1 => M[Res1], child: ReqC => M[ResC])(implicit findReq: FindReq[Res1, Seq[ReqC]]) {
  def into[Res](fn: (Req1, Res1, Seq[(ReqC, ResC)]) => Res): Req1 => M[Res] = {
    enrich.remember(first) andThen (findReq andThen (_.map(reqC => child(reqC).map(resC => (reqC, resC))).flattenM)) finishWith fn
  }
}