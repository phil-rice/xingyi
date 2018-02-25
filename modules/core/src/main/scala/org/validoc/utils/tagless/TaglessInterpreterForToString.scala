package org.validoc.utils.tagless

import org.validoc.utils.cache.{Cachable, ShouldCache}
import org.validoc.utils.http.{ResponseCategoriser, ResponseProcessor, ServiceName, ToServiceRequest}
import org.validoc.utils.logging.{DetailedLogging, SummaryLogging}
import org.validoc.utils.strings.IndentAndString
import org.validoc.utils.success.MessageName

import scala.reflect.ClassTag
import scala.language.higherKinds

class TaglessInterpreterForToString[HttpReq, HttpRes] {

  import org.validoc.utils.reflection.ClassTags._

  type StringHolder[Req, Res] = IndentAndString

  implicit object ForToString extends TaglessLanguage[StringHolder, Void, HttpReq, HttpRes] {
    override def http(name: ServiceName): StringHolder[HttpReq, HttpRes] =
      IndentAndString(0, List()).insertLineAndIndent(s"http(${name.name})")

    override def objectify[Req: ClassTag : ToServiceRequest : ResponseCategoriser, Res: ClassTag](http: StringHolder[HttpReq, HttpRes])(implicit toRequest: ToServiceRequest[Req], categoriser: ResponseCategoriser[Req], responseProcessor: ResponseProcessor[Void, Req, Res]) =
      http.insertLineAndIndent(s"objectify[${nameOf[Req]},${nameOf[Res]}]")

    override def metrics[Req: ClassTag, Res: ClassTag : RD](prefix: String)(raw: StringHolder[Req, Res]) =
      raw.insertLineAndIndent(s"metrics($prefix)")
    override def cache[Req: ClassTag : Cachable : ShouldCache, Res: ClassTag](name: String)(raw: StringHolder[Req, Res]) =
      raw.insertLineAndIndent(s"cache($name)")

    override def logging[Req: ClassTag : DetailedLogging : SummaryLogging, Res: ClassTag : DetailedLogging : SummaryLogging](pattern: String)(raw: StringHolder[Req, Res])(implicit messageName: MessageName[Req, Res]) =
      raw.insertLineAndIndent(s"logging($pattern using prefix ${messageName.name})")

    override protected def enrichPrim[ReqP, ResP, ReqC, ResC, ResE](parent: StringHolder[ReqP, ResP], child: StringHolder[ReqC, ResC])(implicit findChildIds: HasChildren[ResP, ReqC], enricher: Enricher[ReqP, ResP, ReqC, ResC, ResE]) =
      IndentAndString.merge("enrich", parent, child)

    override def merge2Prim[ReqM, ResM, Req1, Res1, Req2, Res2](firstService: StringHolder[Req1, Res1], secondService: StringHolder[Req2, Res2], merger: (ReqM, Res1, Res2) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2): StringHolder[ReqM, ResM] =
      IndentAndString.merge("merge2", firstService, secondService)

    override def merge3Prim[ReqM, ResM, Req1, Res1, Req2, Res2, Req3, Res3](firstService: StringHolder[Req1, Res1], secondService: StringHolder[Req2, Res2], thirdService: StringHolder[Req3, Res3], merger: (ReqM, Res1, Res2, Res3) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3): StringHolder[ReqM, ResM] =
      IndentAndString.merge("merge3", firstService, secondService, thirdService)

    override def merge4Prim[ReqM, ResM, Req1, Res1, Req2, Res2, Req3, Res3, Req4, Res4](firstService: StringHolder[Req1, Res1], secondService: StringHolder[Req2, Res2], thirdService: StringHolder[Req3, Res3], fourthService: StringHolder[Req4, Res4], merger: (ReqM, Res1, Res2, Res3, Res4) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3, reqMtoReq4: ReqM => Req4): StringHolder[ReqM, ResM] =
      IndentAndString.merge("merge4", firstService, secondService, thirdService, fourthService)

  }

}
