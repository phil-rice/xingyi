package org.validoc.tagless

import org.validoc.utils.aggregate.{Enricher, HasChildren}
import org.validoc.utils.cache.{CachableKey, ShouldCacheResult, ShouldUseCache}
import org.validoc.utils.endpoint.MatchesServiceRequest
import org.validoc.utils.functions.Monad
import org.validoc.utils.http._
import org.validoc.utils.logging.{DetailedLogging, SummaryLogging}
import org.validoc.utils.profiling.TryProfileData
import org.validoc.utils.retry.{NeedsRetry, RetryConfig}

import scala.collection.concurrent.TrieMap
import scala.language.higherKinds
import scala.reflect.ClassTag

object DelegatesTaglessLanguage {
}

class DelegatesTaglessLanguage[Endpoint[_, _], Wrapper[_, _], M[_], Fail](interpreter: TaglessLanguage[Endpoint, Wrapper, M, Fail]) extends TaglessLanguage[Endpoint, Wrapper, M, Fail] {
  override def endpoint[Req: ClassTag, Res: ClassTag](normalisedPath: String, matchesServiceRequest: MatchesServiceRequest)(raw: Wrapper[Req, Res])(implicit fromServiceRequest: FromServiceRequest[M, Req], toServiceResponse: ToServiceResponse[Res]) =
    interpreter.endpoint[Req, Res](normalisedPath, matchesServiceRequest)(raw)
  override def chain(endpoints: Endpoint[_, _]*) = interpreter.chain(endpoints: _*)
  override def http(name: ServiceName) = interpreter.http(name)
  override def objectify[Req: ClassTag, Res: ClassTag](http: Wrapper[ServiceRequest, ServiceResponse])(implicit toRequest: ToServiceRequest[Req], categoriser: ResponseCategoriser[M, Req], responseParser: ResponseParser[Fail, Req, Res]) =
    interpreter.objectify(http)
  override def logging[Req: ClassTag : DetailedLogging : SummaryLogging, Res: ClassTag : DetailedLogging : SummaryLogging](messagePrefix: String)(raw: Wrapper[Req, Res]) =
    interpreter.logging(messagePrefix)(raw)
  override def metrics[Req: ClassTag, Res: ClassTag : RD](prefix: String)(raw: Wrapper[Req, Res]) =
    interpreter.metrics(prefix)(raw)
  override def cache[Req: ClassTag : CachableKey : ShouldUseCache, Res: ClassTag : ShouldCacheResult](name: String)(raw: Wrapper[Req, Res]) =
    interpreter.cache(name)(raw)
  override def retry[Req: ClassTag, Res: ClassTag](retryConfig: RetryConfig)(raw: Wrapper[Req, Res])(implicit retry: NeedsRetry[Fail, Res]) =
    interpreter.retry(retryConfig)(raw)
  override def profile[Req: ClassTag, Res: ClassTag](profileData: TryProfileData)(raw: Wrapper[Req, Res]) =
    interpreter.profile(profileData)(raw)
  override def enrichPrim[ReqP: ClassTag, ResP, ReqC, ResC, ResE: ClassTag](parent: Wrapper[ReqP, ResP], child: Wrapper[ReqC, ResC])(implicit findChildIds: HasChildren[ResP, ReqC], enricher: Enricher[ReqP, ResP, ReqC, ResC, ResE]) =
    interpreter.enrichPrim(parent, child)
  override def merge2Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2](firstService: Wrapper[Req1, Res1], secondService: Wrapper[Req2, Res2], merger: (ReqM, Res1, Res2) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2) =
    interpreter.merge2Prim(firstService, secondService, merger)
  override def merge3Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2, Req3, Res3](firstService: Wrapper[Req1, Res1], secondService: Wrapper[Req2, Res2], thirdService: Wrapper[Req3, Res3], merger: (ReqM, Res1, Res2, Res3) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3) =
    interpreter.merge3Prim(firstService, secondService, thirdService, merger)
  override def merge4Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2, Req3, Res3, Req4, Res4](firstService: Wrapper[Req1, Res1], secondService: Wrapper[Req2, Res2], thirdService: Wrapper[Req3, Res3], fourthService: Wrapper[Req4, Res4], merger: (ReqM, Res1, Res2, Res3, Res4) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3, reqMtoReq4: ReqM => Req4) =
    interpreter.merge4Prim(firstService, secondService, thirdService, fourthService, merger)
  override def function[Req: ClassTag, Res: ClassTag](name: String)(fn: Req => Res) =
    interpreter.function(name)(fn)
}
//
//trait WrapperTransform[E[_, _], W[_, _]] {
//  def apply[Req, Res](typeName: String, w: W[Req, Res])(implicit reqClass: ClassTag[Req], resCless: ClassTag[Res]): W[Req, Res]
//  def apply[Req, Res](typeName: String, w: E[Req, Res])(implicit reqClass: ClassTag[Req], resCless: ClassTag[Res]): E[Req, Res]
//}

//case class ProfileTree[M[_], Req, Res](profileData:TryProfileData, klesili: Req => M[Res], children: Seq[ProfileTree[M, _, _]])(implicit reqClass: ClassTag[Req], resCless: ClassTag[Res])
//
//class ProfileEverythingInterpreter[EndPoint[_, _], Wrapper[_, _], M[_], Fail] {
//
//
//  import org.validoc.utils.language.Language._
//
//  case class ProfilerWrapper(interpreter: TaglessLanguage[EndPoint, Wrapper, M, Fail]) extends WrapperTransform[EndPoint, Wrapper] {
//    override def apply[Req, Res](typeName: String, w: Wrapper[Req, Res])(implicit reqClass: ClassTag[Req], resCless: ClassTag[Res]) = {
//      withValue(new TryProfileData) { td => MonitoringInfo(interpreter.profile(td), td) }
//      MonitoringInfo(interpreter.profile()(w))
//    }
//
//  }
//
//  //  def wrapWithProfiler(interpreter: TaglessLanguage[EndPoint, Wrapper, M, Fail]) =
//  //    new Language(interpreter)
//
//  case class Language(interpreter: TaglessLanguage[EndPoint, Wrapper, M, Fail],
//                      wrapEndPoint: WrapperTransform[EndPoint],
//                      wrapService: WrapperTransform[Wrapper]) extends TaglessLanguage[EndPoint, Wrapper, M, Fail] {
//    implicit val i = interpreter
//
//    override def endpoint[Req: ClassTag, Res: ClassTag](normalisedPath: String, matchesServiceRequest: MatchesServiceRequest)(raw: Wrapper[Req, Res])(implicit fromServiceRequest: FromServiceRequest[M, Req], toServiceResponse: ToServiceResponse[Res]) =
//      wrapEndPoint("endpoint", interpreter.endpoint[Req, Res](normalisedPath, matchesServiceRequest)(raw))
//    override def chain(endpoints: EndPoint[_, _]*) =
//      wrapService("chain", interpreter.chain(endpoints: _*))
//    override def http(name: ServiceName) =
//      wrapService("http", interpreter.http(name))
//    override def objectify[Req: ClassTag, Res: ClassTag](http: Wrapper[ServiceRequest, ServiceResponse])(implicit toRequest: ToServiceRequest[Req], categoriser: ResponseCategoriser[M, Req], responseParser: ResponseParser[Fail, Req, Res]) =
//      wrapService("objectify", interpreter.objectify(http))
//    override def logging[Req: ClassTag : DetailedLogging : SummaryLogging, Res: ClassTag : DetailedLogging : SummaryLogging](messagePrefix: String)(raw: Wrapper[Req, Res]) =
//      wrapService("logging", interpreter.logging(messagePrefix)(raw))
//    override def metrics[Req: ClassTag, Res: ClassTag : RD](prefix: String)(raw: Wrapper[Req, Res]) =
//      wrapService("metrics", interpreter.metrics(prefix)(raw))
//    override def cache[Req: ClassTag : CachableKey : ShouldUseCache, Res: ClassTag : ShouldCacheResult](name: String)(raw: Wrapper[Req, Res]) =
//      wrapService("cache", interpreter.cache(name)(raw))
//    override def retry[Req: ClassTag, Res: ClassTag](retryConfig: RetryConfig)(raw: Wrapper[Req, Res])(implicit retry: NeedsRetry[Fail, Res]) =
//      wrapService("retry", interpreter.retry(retryConfig)(raw))
//    override def profile[Req: ClassTag, Res: ClassTag](profileData: TryProfileData)(raw: Wrapper[Req, Res]) =
//      wrapService("profile", interpreter.profile(profileData)(raw))
//    override def enrichPrim[ReqP: ClassTag, ResP, ReqC, ResC, ResE: ClassTag](parent: Wrapper[ReqP, ResP], child: Wrapper[ReqC, ResC])(implicit findChildIds: HasChildren[ResP, ReqC], enricher: Enricher[ReqP, ResP, ReqC, ResC, ResE]) =
//      wrapService("enrich", interpreter.enrichPrim(parent, child))
//    override def merge2Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2](firstService: Wrapper[Req1, Res1], secondService: Wrapper[Req2, Res2], merger: (ReqM, Res1, Res2) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2) =
//      wrapService("merge2", interpreter.merge2Prim(firstService, secondService, merger))
//    override def merge3Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2, Req3, Res3](firstService: Wrapper[Req1, Res1], secondService: Wrapper[Req2, Res2], thirdService: Wrapper[Req3, Res3], merger: (ReqM, Res1, Res2, Res3) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3) =
//      wrapService("merge3", interpreter.merge3Prim(firstService, secondService, thirdService, merger))
//    override def merge4Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2, Req3, Res3, Req4, Res4](firstService: Wrapper[Req1, Res1], secondService: Wrapper[Req2, Res2], thirdService: Wrapper[Req3, Res3], fourthService: Wrapper[Req4, Res4], merger: (ReqM, Res1, Res2, Res3, Res4) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3, reqMtoReq4: ReqM => Req4) =
//      wrapService("merge4", interpreter.merge4Prim(firstService, secondService, thirdService, fourthService, merger))
//    override def function[Req: ClassTag, Res: ClassTag](name: String)(fn: Req => Res) =
//      wrapService("function", interpreter.function(name)(fn))
//  }
//}
//
//
class ProfileEachEndpointLanguage[Endpoint[_, _], Wrapper[_, _], M[_] : Monad, Fail](interpreter: TaglessLanguage[Endpoint, Wrapper, M, Fail]) extends DelegatesTaglessLanguage[Endpoint, Wrapper, M, Fail](interpreter) {
  //TODO So here we have an interesting example of where a State monad would actually help. I think it would mean we didn't have mutable code and the interpreter wasn't stateless
  //This code is remarkably easier though...
  val map = TrieMap[String, TryProfileData]()
  override def endpoint[Req: ClassTag, Res: ClassTag](normalisedPath: String, matchesServiceRequest: MatchesServiceRequest)(raw: Wrapper[Req, Res])(implicit fromServiceRequest: FromServiceRequest[M, Req], toServiceResponse: ToServiceResponse[Res]) = {
    val data = map.getOrElseUpdate(normalisedPath + matchesServiceRequest, new TryProfileData)
    interpreter.endpoint(normalisedPath, matchesServiceRequest)(raw |+| profile(data))
  }

  def dump = ServiceResponse(Status(200), Body(map.map { case (k, v) => (f"$k%-40s" + " " + v.toShortString).replaceAll(" ", "&nbsp;") }.mkString("<br />")), ContentType("text/html "))

  def profileMetricsEndpoint = function[ServiceRequest, ServiceResponse]("profileMetricsEndpoint")(_ => dump) |++| endpoint[ServiceRequest, ServiceResponse]("/profile", MatchesServiceRequest.fixedPath(Get))

}
//
//
//case class WrapperAndDebugInfo[Wrapper[_, _], M[_], Req, Res: ClassTag](wrapper: Wrapper[Req, Res], debugInfo: HasDebugInfo[M, Req, Res])(implicit val reqClassTag: ClassTag[Res])
//
//class DebugEachObjectifyEndpoint[Endpoint[_, _], Wrapper[_, _], M[_] : Monad, Fail](interpreter: TaglessLanguage[Endpoint, Wrapper, M, Fail]) extends DelegatesTaglessLanguage[Endpoint, Wrapper, M, Fail](interpreter) {
//  val map = TrieMap[DebugInfo[M, _, _], WrapperAndDebugInfo[Wrapper, M, _, _]]()
//  def dump = map.values.map { case d@WrapperAndDebugInfo(t, f) => d.reqClassTag.getClass.getSimpleName }.mkString("\n")
//  override def objectify[Req: ClassTag, Res: ClassTag](http: Wrapper[ServiceRequest, ServiceResponse])(implicit toRequest: ToServiceRequest[Req], categoriser: ResponseCategoriser[M, Req], responseParser: ResponseParser[Fail, Req, Res], debugInfo: DebugInfo[M, Req, Res]) = {
//    val result = interpreter.objectify[Req, Res](http)
//    debugInfo match {
//      case d@HasDebugInfo(from, to) => map.getOrElseUpdate(d, WrapperAndDebugInfo(result, d))
//      case _ =>
//    }
//    result
//  }
//}
