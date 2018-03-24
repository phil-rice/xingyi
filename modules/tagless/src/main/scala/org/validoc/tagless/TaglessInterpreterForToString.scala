package org.validoc.tagless

import org.validoc.utils.aggregate.{Enricher, HasChildren}
import org.validoc.utils.cache.{CachableKey, ShouldCacheResult, ShouldUseCache}
import org.validoc.utils.endpoint.MatchesServiceRequest
import org.validoc.utils.functions.{Liftable, MonadCanFail}
import org.validoc.utils.http._
import org.validoc.utils.logging.{DetailedLogging, SummaryLogging}
import org.validoc.utils.metrics.ReportData
import org.validoc.utils.profiling.{ProfileAs, TryProfileData}
import org.validoc.utils.retry.{NeedsRetry, RetryConfig}
import org.validoc.utils.strings.IndentAnd

import scala.language.higherKinds
import scala.reflect.ClassTag

object TaglessInterpreterForToString {
  val structureEndpointName = "structure"
  val defaultStructureEndpoint = "/debug"


  type StringHolder[Req, Res] = IndentAnd[String]

  def forToString[M[_]:Liftable]: TaglessInterpreterForToString#ForToString[M] = new TaglessInterpreterForToString().forToString[M]

  def systemToString[M[_], Fail](block: TaglessLanguage[StringHolder, M] => StringHolder[ServiceRequest, ServiceResponse])(implicit monadCanFail: MonadCanFail[M, Fail]): String = {
    block(forToString[M]).invertIndent.defaultToString("&nbsp;&nbsp;", "<br />")
  }

  def systemHtmlEndpoint[Wrapper[_, __], M[_], Fail](endpointPath: String, language: TaglessLanguage[Wrapper, M])(block: TaglessLanguage[StringHolder, M] => StringHolder[ServiceRequest, ServiceResponse])(implicit monadCanFail: MonadCanFail[M, Fail]): Wrapper[ServiceRequest, Option[ServiceResponse]] = {
    import language._
    val html = systemToString[M, Fail](block)
    val htmlFn = { s: ServiceRequest => ServiceResponse(Status(200), Body(html), ContentType("text/html")) }
    function[ServiceRequest, ServiceResponse]("html")(htmlFn) |+| endpoint[ServiceRequest, ServiceResponse](endpointPath, MatchesServiceRequest.fixedPath(Get))
  }
}

class TaglessInterpreterForToString {


  import TaglessInterpreterForToString._
  import org.validoc.utils.reflection.ClassTags._

  def forToString[M[_]:Liftable] = new ForToString[M]


  class ForToString[M[_] : Liftable] extends TaglessLanguage[StringHolder, M] {
    override def function[Req: ClassTag, Res: ClassTag](name: String)(fn: Req => Res) =
      IndentAnd(0, List()).insertLineAndIndent(s"function-$name")
    override def http(name: ServiceName): StringHolder[ServiceRequest, ServiceResponse] =
      IndentAnd(0, List()).insertLineAndIndent(s"http(${name.name})")
    override def objectify[Req: ClassTag : DetailedLogging, Res: ClassTag](http: StringHolder[ServiceRequest, ServiceResponse])(implicit toRequest: ToServiceRequest[Req], categoriser: ResponseCategoriser[Req], responseProcessor: ResponseParser[Req, Res]): StringHolder[Req, Res] =
      http.insertLineAndIndent(s"objectify[${nameOf[Req]},${nameOf[Res]}]")
    override def andAfter[Req: ClassTag, Mid: ClassTag, Res2: ClassTag](raw: StringHolder[Req, Mid], fn: Mid => Res2): StringHolder[Req, Res2] =
      raw.insertLineAndIndent(s"andAfter[${nameOf[Mid]},${nameOf[Res2]}]")
    override def andAfterK[Req: ClassTag, Mid: ClassTag, Res2: ClassTag](raw: StringHolder[Req, Mid], fn: Mid => M[Res2]): StringHolder[Req, Res2] =
      raw.insertLineAndIndent(s"andAfterK[${nameOf[Mid]},${nameOf[Res2]}]")
    override def chain(endpoints: StringHolder[ServiceRequest, ServiceResponse]*) =
      IndentAnd.merge("chain", endpoints: _*)
    override def endpoint[Req: ClassTag, Res: ClassTag](normalisedPath: String, matchesServiceRequest: MatchesServiceRequest)(raw: StringHolder[Req, Res])(implicit fromServiceRequest: FromServiceRequest[M, Req], toServiceResponse: ToServiceResponse[Res]): StringHolder[Req, Res] =
      raw.insertLineAndIndent(s"endpoint[${nameOf[Req]},${nameOf[Res]}]($normalisedPath,$matchesServiceRequest)")
    override def debugEndpoints(endpoints: Map[String, String])(original: StringHolder[ServiceRequest, Option[ServiceResponse]]) = {
      import language._
      val endpointPath = endpoints.get(TaglessInterpreterForToString.structureEndpointName).getOrElse(TaglessInterpreterForToString.defaultStructureEndpoint)
      val html = original.invertIndent.defaultToString("&nbsp;&nbsp;", "<br />")
      val htmlFn = { s: ServiceRequest => ServiceResponse(Status(200), Body(html), ContentType("text/html")) }
      function[ServiceRequest, ServiceResponse]("html")(htmlFn) |+| endpoint[ServiceRequest, ServiceResponse](endpointPath, MatchesServiceRequest.fixedPath(Get))
    }

    override def metrics[Req: ClassTag, Res: ClassTag : ReportData](prefix: String)(raw: StringHolder[Req, Res]) =
      raw.insertLineAndIndent(s"metrics($prefix)")
    override def cache[Req: ClassTag : CachableKey : ShouldUseCache, Res: ClassTag : ShouldCacheResult](name: String)(raw: StringHolder[Req, Res]) =
      raw.insertLineAndIndent(s"cache($name)")
    override def retry[Req: ClassTag, Res: ClassTag : NeedsRetry](retryConfig: RetryConfig)(raw: StringHolder[Req, Res]): StringHolder[Req, Res] =
      raw.insertLineAndIndent(s"retry($retryConfig)")

    override def profile[Req: ClassTag, Res: ClassTag : ProfileAs](profileData: TryProfileData)(raw: StringHolder[Req, Res]) =
      raw.insertLineAndIndent(s"profile")

    override def logging[Req: ClassTag : DetailedLogging : SummaryLogging, Res: ClassTag : DetailedLogging : SummaryLogging](messagePrefix: String)(raw: StringHolder[Req, Res]) =
      raw.insertLineAndIndent(s"logging(Using $messagePrefix)")

    override def enrichPrim[ReqP: ClassTag, ResP, ReqC, ResC, ResE: ClassTag](parent: StringHolder[ReqP, ResP], child: StringHolder[ReqC, ResC])(implicit findChildIds: HasChildren[ResP, ReqC], enricher: Enricher[ReqP, ResP, ReqC, ResC, ResE]) =
      IndentAnd.merge("enrich", parent, child)

    override def merge2Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2](firstService: StringHolder[Req1, Res1], secondService: StringHolder[Req2, Res2], merger: (ReqM, Res1, Res2) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2): StringHolder[ReqM, ResM] =
      IndentAnd.merge("merge2", firstService, secondService)

    override def merge3Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2, Req3, Res3](firstService: StringHolder[Req1, Res1], secondService: StringHolder[Req2, Res2], thirdService: StringHolder[Req3, Res3], merger: (ReqM, Res1, Res2, Res3) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3): StringHolder[ReqM, ResM] =
      IndentAnd.merge("merge3", firstService, secondService, thirdService)

    override def merge4Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2, Req3, Res3, Req4, Res4](firstService: StringHolder[Req1, Res1], secondService: StringHolder[Req2, Res2], thirdService: StringHolder[Req3, Res3], fourthService: StringHolder[Req4, Res4], merger: (ReqM, Res1, Res2, Res3, Res4) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3, reqMtoReq4: ReqM => Req4): StringHolder[ReqM, ResM] =
      IndentAnd.merge("merge4", firstService, secondService, thirdService, fourthService)

  }

}
