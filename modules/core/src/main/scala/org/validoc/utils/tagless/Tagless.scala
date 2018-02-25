package org.validoc.utils.tagless

import org.validoc.utils.functions.MonadCanFail
import org.validoc.utils.logging._
import org.validoc.utils.metrics.{MetricsService, PutMetrics, ReportData}
import org.validoc.utils.strings.IndentAndString
import org.validoc.utils.success.MessageName
import org.validoc.utils.time.NanoTimeService

import scala.language.higherKinds
import scala.reflect.ClassTag

trait HasChildren[Main, Children] extends (Main => Seq[Children])

trait Enricher[Req, Parent, ChildId, Child, Res] extends ((Req, Parent, Seq[(ChildId, ChildId)]) => Res)

trait TaglessLanguage[Wrapper[_, _], Fail, HttpReq, HttpRes] extends HttpLanguage[Wrapper, Fail, HttpReq, HttpRes] with NonfunctionalLanguage[Wrapper, Fail] with ComposeLanguage[Wrapper] {

}

trait HttpLanguage[Wrapper[_, _], Fail, HttpReq, HttpRes] extends NonfunctionalLanguage[Wrapper, Fail] {
  def http: Wrapper[HttpReq, HttpRes]

  def objectify[Req: ClassTag, Res: ClassTag](http: Wrapper[HttpReq, HttpRes]): Wrapper[Req, Res]
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
  def enrich[ReqP, ResP, ReqC, ResC, ResE](parent: Wrapper[ReqP, ResP], child: Wrapper[ReqC, ResC])(implicit findChildIds: HasChildren[ResP, ReqC], enricher: Enricher[ResE, ResP, ReqC, ResC, ResE]): Wrapper[ReqP, ResE]

  def merge2[ReqM, ResM, Req1, Res1, Req2, Res2](firstService: Wrapper[Req1, Res1], secondService: Wrapper[Req2, Res2], merger: (ReqM, Res1, Res2) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2): Wrapper[ReqM, ResM]

  def merge3[ReqM, ResM, Req1, Res1, Req2, Res2, Req3, Res3](firstService: Wrapper[Req1, Res1], secondService: Wrapper[Req2, Res2], thirdService: Wrapper[Req3, Res3], merger: (ReqM, Res1, Res2, Res3) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3): Wrapper[ReqM, ResM]

  def merge4[ReqM, ResM, Req1, Res1, Req2, Res2, Req3, Res3, Req4, Res4](firstService: Wrapper[Req1, Res1], secondService: Wrapper[Req2, Res2], thirdService: Wrapper[Req3, Res3], fourthService: Wrapper[Req4, Res4], merger: (ReqM, Res1, Res2, Res3, Res4) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3): Wrapper[ReqM, ResM]

}

class TaglessLanguageLanguageForKleislis[M[_], Fail, HttpReq, HttpRes](implicit monadCanFail: MonadCanFail[M, Fail]) {

  type K[Req, Res] = Req => M[Res]

  class NonFunctionalLanguageService(implicit timeService: NanoTimeService, putMetrics: PutMetrics, loggingAdapter: LoggingAdapter, logReqAndResult: LogRequestAndResult[Fail]) extends TaglessLanguage[K, Fail, HttpReq, HttpRes] {
    override def metrics[Req: ClassTag, Res: ClassTag : RD](prefix: String)(raw: K[Req, Res]) =
      new MetricsService[M, Fail].metrics[Req, Res](prefix, raw)
    override def logging[Req: ClassTag : DetailedLogging : SummaryLogging, Res: ClassTag : DetailedLogging : SummaryLogging](pattern: String)(raw: K[Req, Res])(implicit messageName: MessageName[Req, Res]) =
      new LoggingService[M, Fail, Req, Res](raw, pattern).logging(pattern)(raw)
    override def enrich[ReqP, ResP, ReqC, ResC, ResE](parent: K[ReqP, ResP], child: K[ReqC, ResC])(implicit findChildIds: HasChildren[ResP, ReqC], enricher: Enricher[ResE, ResP, ReqC, ResC, ResE]) =
    override def merge2[ReqM, ResM, Req1, Res1, Req2, Res2](firstService: K[Req1, Res1], secondService: K[Req2, Res2], merger: (ReqM, Res1, Res2) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2) = ???
    override def merge3[ReqM, ResM, Req1, Res1, Req2, Res2, Req3, Res3](firstService: K[Req1, Res1], secondService: K[Req2, Res2], thirdService: K[Req3, Res3], merger: (ReqM, Res1, Res2, Res3) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3) = ???
    override def merge4[ReqM, ResM, Req1, Res1, Req2, Res2, Req3, Res3, Req4, Res4](firstService: K[Req1, Res1], secondService: K[Req2, Res2], thirdService: K[Req3, Res3], fourthService: K[Req4, Res4], merger: (ReqM, Res1, Res2, Res3, Res4) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3) = ???
    override def http = ???
    override def objectify[Req: ClassTag, Res: ClassTag](http: K[HttpReq, HttpRes]) = ???
  }
}

class TaglessLanguageForToString[HttpReq, HttpRes] {

  import org.validoc.utils.reflection.ClassTags._

  type StringHolder[Req, Res] = IndentAndString

  implicit object ForToString extends TaglessLanguage[StringHolder, Void, HttpReq, HttpRes] {
    override def http: StringHolder[HttpReq, HttpRes] =
      IndentAndString(0, List()).insertLineAndIndent("http")

    override def objectify[Req: ClassTag, Res: ClassTag](http: StringHolder[HttpReq, HttpRes]): StringHolder[Req, Res] =
      http.insertLineAndIndent(s"objectify[${nameOf[Req]},${nameOf[Res]}]")

    override def metrics[Req: ClassTag, Res: ClassTag : RD](prefix: String)(raw: StringHolder[Req, Res]) =
      raw.insertLineAndIndent(s"metrics($prefix)")

    override def logging[Req: ClassTag : DetailedLogging : SummaryLogging, Res: ClassTag : DetailedLogging : SummaryLogging](pattern: String)(raw: StringHolder[Req, Res])(implicit messageName: MessageName[Req, Res]) =
      raw.insertLineAndIndent(s"logging($pattern using prefix${messageName.name})")

    override def enrich[ReqP, ResP, ReqC, ResC, ResE](parent: StringHolder[ReqP, ResP], child: StringHolder[ReqC, ResC])(implicit findChildIds: HasChildren[ResP, ReqC], enricher: Enricher[ResE, ResP, ReqC, ResC, ResE]): StringHolder[ReqP, ResE] =
      IndentAndString.merge("enrich", parent, child)

    override def merge2[ReqM, ResM, Req1, Res1, Req2, Res2](firstService: StringHolder[Req1, Res1], secondService: StringHolder[Req2, Res2], merger: (ReqM, Res1, Res2) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2): StringHolder[ReqM, ResM] =
      IndentAndString.merge("merge2", firstService, secondService)

    override def merge3[ReqM, ResM, Req1, Res1, Req2, Res2, Req3, Res3](firstService: StringHolder[Req1, Res1], secondService: StringHolder[Req2, Res2], thirdService: StringHolder[Req3, Res3], merger: (ReqM, Res1, Res2, Res3) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3): StringHolder[ReqM, ResM] =
      IndentAndString.merge("merge3", firstService, secondService, thirdService)

    override def merge4[ReqM, ResM, Req1, Res1, Req2, Res2, Req3, Res3, Req4, Res4](firstService: StringHolder[Req1, Res1], secondService: StringHolder[Req2, Res2], thirdService: StringHolder[Req3, Res3], fourthService: StringHolder[Req4, Res4], merger: (ReqM, Res1, Res2, Res3, Res4) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3): StringHolder[ReqM, ResM] =
      IndentAndString.merge("merge4", firstService, secondService, thirdService, fourthService)

  }

}

