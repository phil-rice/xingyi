package org.validoc.utils.tagless

import org.validoc.utils.functions.MonadCanFail
import org.validoc.utils.http._
import org.validoc.utils.logging._
import org.validoc.utils.metrics.{MetricsService, PutMetrics, ReportData}
import org.validoc.utils.strings.IndentAndString
import org.validoc.utils.success.MessageName
import org.validoc.utils.time.NanoTimeService

import scala.language.higherKinds
import scala.reflect.ClassTag

trait HasChildren[Main, Children] extends (Main => Seq[Children])

trait Enricher[Req, Parent, ChildId, Child, Res] extends ((Req, Parent, Seq[(ChildId, Child)]) => Res)

trait TaglessLanguage[Wrapper[_, _], Fail, HttpReq, HttpRes] extends HttpLanguage[Wrapper, Fail, HttpReq, HttpRes] with NonfunctionalLanguage[Wrapper, Fail] with ComposeLanguage[Wrapper]

trait HttpLanguage[Wrapper[_, _], Fail, HttpReq, HttpRes] extends NonfunctionalLanguage[Wrapper, Fail] {
  def http(name: String): Wrapper[HttpReq, HttpRes]
  def objectify[Req: ClassTag : ToServiceRequest : ResponseCategoriser, Res: ClassTag](http: Wrapper[HttpReq, HttpRes])(implicit toRequest: ToServiceRequest[Req], categoriser: ResponseCategoriser[Req], responseProcessor: ResponseProcessor[Fail, Req, Res]): Wrapper[Req, Res]
}

trait NonfunctionalLanguage[Wrapper[_, _], Fail] {
  type RD[T] = ReportData[Fail, T]
  def metrics[Req: ClassTag, Res: ClassTag : RD](prefix: String)(raw: Wrapper[Req, Res]): Wrapper[Req, Res]

  def logging[Req: ClassTag : DetailedLogging : SummaryLogging, Res: ClassTag : DetailedLogging : SummaryLogging](pattern: String)(raw: Wrapper[Req, Res])(implicit messageName: MessageName[Req, Res]): Wrapper[Req, Res]


  implicit class ComposePimper[RawReq, RawRes](wrapper: Wrapper[RawReq, RawRes]) {
    def |+|[Req, Res](fn: Wrapper[RawReq, RawRes] => Wrapper[Req, Res]): Wrapper[Req, Res] = fn(wrapper)
  }

}

trait ComposeLanguage[Wrapper[_, _]] {
  case class enrich[ReqP, ResP](parent: Wrapper[ReqP, ResP]) {
    case class withChild[ReqC, ResC](child: Wrapper[ReqC, ResC])(implicit findChildIds: HasChildren[ResP, ReqC]) {
      def mergeInto[ResE](implicit enricher: Enricher[ReqP, ResP, ReqC, ResC, ResE]): Wrapper[ReqP, ResE] = enrichPrim(parent, child)
    }
  }

  protected def enrichPrim[ReqP, ResP, ReqC, ResC, ResE](parent: Wrapper[ReqP, ResP], child: Wrapper[ReqC, ResC])(implicit findChildIds: HasChildren[ResP, ReqC], enricher: Enricher[ReqP, ResP, ReqC, ResC, ResE]): Wrapper[ReqP, ResE]

  def merge2[ReqM, ResM, Req1, Res1, Req2, Res2](firstService: Wrapper[Req1, Res1], secondService: Wrapper[Req2, Res2], merger: (ReqM, Res1, Res2) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2): Wrapper[ReqM, ResM]

  def merge3[ReqM, ResM, Req1, Res1, Req2, Res2, Req3, Res3](firstService: Wrapper[Req1, Res1], secondService: Wrapper[Req2, Res2], thirdService: Wrapper[Req3, Res3], merger: (ReqM, Res1, Res2, Res3) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3): Wrapper[ReqM, ResM]

  def merge4[ReqM, ResM, Req1, Res1, Req2, Res2, Req3, Res3, Req4, Res4](firstService: Wrapper[Req1, Res1], secondService: Wrapper[Req2, Res2], thirdService: Wrapper[Req3, Res3], fourthService: Wrapper[Req4, Res4], merger: (ReqM, Res1, Res2, Res3, Res4) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3): Wrapper[ReqM, ResM]

}





