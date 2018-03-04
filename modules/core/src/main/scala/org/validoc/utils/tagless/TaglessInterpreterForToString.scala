package org.validoc.utils.tagless

import org.validoc.utils.cache.{Cachable, ShouldCache}
import org.validoc.utils.endpoint.MatchesServiceRequest
import org.validoc.utils.functions.MonadCanFail
import org.validoc.utils.http._
import org.validoc.utils.logging.{DetailedLogging, SummaryLogging}
import org.validoc.utils.profiling.TryProfileData
import org.validoc.utils.retry.{NeedsRetry, RetryConfig}
import org.validoc.utils.strings.IndentAndString

import scala.language.higherKinds
import scala.reflect.ClassTag

class TaglessInterpreterForToString {


  import org.validoc.utils.reflection.ClassTags._

  type StringHolder[Req, Res] = IndentAndString

  def systemToString[M[_], Fail](fn: TaglessLanguage[StringHolder, StringHolder, M, Fail] => StringHolder[ServiceRequest, ServiceResponse])(implicit monadCanFail: MonadCanFail[M, Fail]) = {
    val toStringLanguage = new ForToString[M, Fail]()
    fn(toStringLanguage).invertIndent.toString("&nbsp;&nbsp;", "<br />")
  }

  def systemToEndpoint[EndpointWrapper[_, _], Wrapper[_, __], M[_], Fail](endpointPath: String, language: TaglessLanguage[EndpointWrapper, Wrapper, M, Fail])(fn: TaglessLanguage[StringHolder, StringHolder, M, Fail] => StringHolder[ServiceRequest, ServiceResponse])(implicit monadCanFail: MonadCanFail[M, Fail]) = {
    val html = systemToString[M, Fail](fn)

    import language._

    val htmlFn = { s: ServiceRequest => ServiceResponse(Status(200), Body(html), ContentType("text/html")) }
    function[ServiceRequest, ServiceResponse]("html")(htmlFn) |++| endpoint[ServiceRequest, ServiceResponse](endpointPath, MatchesServiceRequest.fixedPath(Get))
  }

  implicit def forToString[M[_], Fail] = new ForToString[M, Fail]
  class ForToString[M[_], Fail] extends TaglessLanguage[StringHolder, StringHolder, M, Fail] {
    override def function[Req, Res](name: String)(fn: Req => Res) =
      IndentAndString(0, List()).insertLineAndIndent(s"function-$name)")

    override def http(name: ServiceName): StringHolder[ServiceRequest, ServiceResponse] =
      IndentAndString(0, List()).insertLineAndIndent(s"http(${name.name})")
    override def objectify[Req: ClassTag, Res: ClassTag](http: StringHolder[ServiceRequest, ServiceResponse])(implicit toRequest: ToServiceRequest[Req], categoriser: ResponseCategoriser[M, Req], responseParser: ResponseParser[Fail, Req, Res]) =
      http.insertLineAndIndent(s"objectify[${nameOf[Req]},${nameOf[Res]}]")
    override def chain(endpoints: StringHolder[_, _]*) =
      IndentAndString.merge("chain", endpoints: _*)
    override def endpoint[Req: ClassTag, Res: ClassTag](normalisedPath: String, matchesServiceRequest: MatchesServiceRequest)(raw: StringHolder[Req, Res])(implicit fromServiceRequest: FromServiceRequest[M, Req], toServiceResponse: ToServiceResponse[Res]): StringHolder[Req, Res] =
      raw.insertLineAndIndent(s"endpoint[${nameOf[Req]},${nameOf[Res]}]($normalisedPath,$matchesServiceRequest)")
    override def metrics[Req: ClassTag, Res: ClassTag : RD](prefix: String)(raw: StringHolder[Req, Res]) =
      raw.insertLineAndIndent(s"metrics($prefix)")
    override def cache[Req: ClassTag : Cachable : ShouldCache, Res: ClassTag](name: String)(raw: StringHolder[Req, Res]) =
      raw.insertLineAndIndent(s"cache($name)")
    override def retry[Req: ClassTag, Res: ClassTag](retryConfig: RetryConfig)(raw: StringHolder[Req, Res])(implicit retry: NeedsRetry[Fail, Res]): StringHolder[Req, Res] =
      raw.insertLineAndIndent(s"retry($retryConfig)")

    override def profile[Req: ClassTag, Res: ClassTag](profileData: TryProfileData)(raw: StringHolder[Req, Res]) =
      raw.insertLineAndIndent(s"profile")

    override def logging[Req: ClassTag : DetailedLogging : SummaryLogging, Res: ClassTag : DetailedLogging : SummaryLogging](messagePrefix: String)(raw: StringHolder[Req, Res]) =
      raw.insertLineAndIndent(s"logging(Using $messagePrefix)")

    override def enrichPrim[ReqP, ResP, ReqC, ResC, ResE](parent: StringHolder[ReqP, ResP], child: StringHolder[ReqC, ResC])(implicit findChildIds: HasChildren[ResP, ReqC], enricher: Enricher[ReqP, ResP, ReqC, ResC, ResE]) =
      IndentAndString.merge("enrich", parent, child)

    override def merge2Prim[ReqM, ResM, Req1, Res1, Req2, Res2](firstService: StringHolder[Req1, Res1], secondService: StringHolder[Req2, Res2], merger: (ReqM, Res1, Res2) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2): StringHolder[ReqM, ResM] =
      IndentAndString.merge("merge2", firstService, secondService)

    override def merge3Prim[ReqM, ResM, Req1, Res1, Req2, Res2, Req3, Res3](firstService: StringHolder[Req1, Res1], secondService: StringHolder[Req2, Res2], thirdService: StringHolder[Req3, Res3], merger: (ReqM, Res1, Res2, Res3) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3): StringHolder[ReqM, ResM] =
      IndentAndString.merge("merge3", firstService, secondService, thirdService)

    override def merge4Prim[ReqM, ResM, Req1, Res1, Req2, Res2, Req3, Res3, Req4, Res4](firstService: StringHolder[Req1, Res1], secondService: StringHolder[Req2, Res2], thirdService: StringHolder[Req3, Res3], fourthService: StringHolder[Req4, Res4], merger: (ReqM, Res1, Res2, Res3, Res4) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3, reqMtoReq4: ReqM => Req4): StringHolder[ReqM, ResM] =
      IndentAndString.merge("merge4", firstService, secondService, thirdService, fourthService)

  }

}
