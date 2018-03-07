package org.validoc.tagless

import org.validoc.utils.aggregate.{Enricher, HasChildren}
import org.validoc.utils.cache.{CachableKey, ShouldCacheResult, ShouldUseCache}
import org.validoc.utils.endpoint.MatchesServiceRequest
import org.validoc.utils.functions.{Monad, MonadWithException}
import org.validoc.utils.http._
import org.validoc.utils.logging.{DetailedLogging, SummaryLogging}
import org.validoc.utils.profiling.{ProfileKleisli, TryProfileData}
import org.validoc.utils.retry.{NeedsRetry, RetryConfig}
import org.validoc.utils.time.NanoTimeService

import scala.collection.concurrent.TrieMap
import scala.language.higherKinds
import scala.reflect.ClassTag


class Profile2[M[_] : MonadWithException, Fail] {
  type Kleisli[Req, Res] = Req => M[Res]

  //TODO Need to add children to this so that we can do an HTML dump of it.
  // But this looks like it will work!
  //So this wraps every node in out tree, and we should be able to see the web page with the profiles of everything!
  case class ProfilingWrapper[Req: ClassTag, Res: ClassTag](name: String, kleisli: Req => M[Res])(implicit nanoTimeService: NanoTimeService) extends PartialFunction[Req, M[Res]] {
    val tryProfileData = new TryProfileData
    override def apply(v1: Req) = ProfileKleisli(tryProfileData)(kleisli) apply v1
    override def isDefinedAt(x: Req) = kleisli match {
      case pf: PartialFunction[Req, M[Res]] => pf.isDefinedAt(x)
      case _ => true
    }
  }

  case class Language(interpreter: TaglessLanguage[Kleisli, M, Fail]) extends TaglessLanguage[ProfilingWrapper, M, Fail] {
    override def http(name: ServiceName) =
      ProfilingWrapper("http", interpreter.http(name))
    override def function[Req: ClassTag, Res: ClassTag](name: String)(fn: Req => Res) =
      ProfilingWrapper("function", interpreter.function(name)(fn))

    override def logging[Req: ClassTag : DetailedLogging : SummaryLogging, Res: ClassTag : DetailedLogging : SummaryLogging](messagePrefix: String)(raw: ProfilingWrapper[Req, Res]) =
      ProfilingWrapper("logging", interpreter.logging(messagePrefix)(raw))
    override def metrics[Req: ClassTag, Res: ClassTag : RD](prefix: String)(raw: ProfilingWrapper[Req, Res]) =
      ProfilingWrapper("metrics", interpreter.metrics(prefix)(raw))
    override def cache[Req: ClassTag : CachableKey : ShouldUseCache, Res: ClassTag : ShouldCacheResult](name: String)(raw: ProfilingWrapper[Req, Res]) =
      ProfilingWrapper("cache", interpreter.cache(name)(raw))
    override def retry[Req: ClassTag, Res: ClassTag](retryConfig: RetryConfig)(raw: ProfilingWrapper[Req, Res])(implicit retry: NeedsRetry[Fail, Res]) =
      ProfilingWrapper("retry", interpreter.retry(retryConfig)(raw))
    override def profile[Req: ClassTag, Res: ClassTag](profileData: TryProfileData)(raw: ProfilingWrapper[Req, Res]) =
      ProfilingWrapper("profile", interpreter.profile(profileData)(raw))
    override def chain(endpoints: ProfilingWrapper[ServiceRequest, ServiceResponse]*) =
      ProfilingWrapper("chain", interpreter.chain(endpoints: _*))
    override def objectify[Req: ClassTag, Res: ClassTag](http: ProfilingWrapper[ServiceRequest, ServiceResponse])(implicit toRequest: ToServiceRequest[Req], categoriser: ResponseCategoriser[M, Req], responseParser: ResponseParser[Fail, Req, Res]) =
      ProfilingWrapper("objectify", interpreter.objectify(http))

    //TODO Is this a compiler bug the need for the asInstanceOf???
    override def endpoint[Req: ClassTag, Res: ClassTag](normalisedPath: String, matchesServiceRequest: MatchesServiceRequest)(raw: ProfilingWrapper[Req, Res])(implicit fromServiceRequest: FromServiceRequest[M, Req], toServiceResponse: ToServiceResponse[Res]) =
      ProfilingWrapper("endpoint", interpreter.endpoint(normalisedPath, matchesServiceRequest)(raw.asInstanceOf[Kleisli[Req, Res]]))
    //
    override def enrichPrim[ReqP: ClassTag, ResP, ReqC, ResC, ResE: ClassTag](parent: ProfilingWrapper[ReqP, ResP], child: ProfilingWrapper[ReqC, ResC])(implicit findChildIds: HasChildren[ResP, ReqC], enricher: Enricher[ReqP, ResP, ReqC, ResC, ResE]) =
      ProfilingWrapper("enrich", interpreter.enrichPrim(parent, child.asInstanceOf[Kleisli[ReqC, ResC]]))
    override def merge2Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2](firstService: ProfilingWrapper[Req1, Res1], secondService: ProfilingWrapper[Req2, Res2], merger: (ReqM, Res1, Res2) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2) =
      ProfilingWrapper("merge2", interpreter.merge2Prim(firstService.asInstanceOf[Kleisli[Req1, Res1]], secondService.asInstanceOf[Kleisli[Req2, Res2]], merger))
    override def merge3Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2, Req3, Res3](firstService: ProfilingWrapper[Req1, Res1], secondService: ProfilingWrapper[Req2, Res2], thirdService: ProfilingWrapper[Req3, Res3], merger: (ReqM, Res1, Res2, Res3) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3) =
      ProfilingWrapper("merge3", interpreter.merge3Prim(firstService.asInstanceOf[Kleisli[Req1, Res1]], secondService.asInstanceOf[Kleisli[Req2, Res2]], thirdService.asInstanceOf[Kleisli[Req3, Res3]], merger))
    override def merge4Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2, Req3, Res3, Req4, Res4](firstService: ProfilingWrapper[Req1, Res1], secondService: ProfilingWrapper[Req2, Res2], thirdService: ProfilingWrapper[Req3, Res3], fourthService: ProfilingWrapper[Req4, Res4], merger: (ReqM, Res1, Res2, Res3, Res4) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3, reqMtoReq4: ReqM => Req4) =
      ProfilingWrapper("merge4", interpreter.merge4Prim(firstService.asInstanceOf[Kleisli[Req1, Res1]], secondService.asInstanceOf[Kleisli[Req2, Res2]], thirdService.asInstanceOf[Kleisli[Req3, Res3]], fourthService.asInstanceOf[Kleisli[Req4, Res4]], merger))
  }

}

object DelegatesTaglessLanguage {
}

class DelegatesTaglessLanguage[Wrapper[_, _], M[_], Fail](interpreter: TaglessLanguage[Wrapper, M, Fail]) extends TaglessLanguage[Wrapper, M, Fail] {
  override def endpoint[Req: ClassTag, Res: ClassTag](normalisedPath: String, matchesServiceRequest: MatchesServiceRequest)(raw: Wrapper[Req, Res])(implicit fromServiceRequest: FromServiceRequest[M, Req], toServiceResponse: ToServiceResponse[Res]) =
    interpreter.endpoint[Req, Res](normalisedPath, matchesServiceRequest)(raw)
  override def chain(endpoints: Wrapper[ServiceRequest, ServiceResponse]*) = interpreter.chain(endpoints: _*)
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
class ProfileEachEndpointLanguage[Wrapper[_, _], M[_] : Monad, Fail](interpreter: TaglessLanguage[Wrapper, M, Fail]) extends DelegatesTaglessLanguage[Wrapper, M, Fail](interpreter) {
  //TODO So here we have an interesting example of where a State monad would actually help. I think it would mean we didn't have mutable code and the interpreter wasn't stateless
  //This code is remarkably easier though...
  val map = TrieMap[String, TryProfileData]()
  override def endpoint[Req: ClassTag, Res: ClassTag](normalisedPath: String, matchesServiceRequest: MatchesServiceRequest)(raw: Wrapper[Req, Res])(implicit fromServiceRequest: FromServiceRequest[M, Req], toServiceResponse: ToServiceResponse[Res]) = {
    val data = map.getOrElseUpdate(normalisedPath + matchesServiceRequest, new TryProfileData)
    interpreter.endpoint(normalisedPath, matchesServiceRequest)(raw |+| profile(data))
  }

  def dump = ServiceResponse(Status(200), Body(map.map { case (k, v) => (f"$k%-40s" + " " + v.toShortString).replaceAll(" ", "&nbsp;") }.mkString("<br />")), ContentType("text/html "))

  def profileMetricsEndpoint = function[ServiceRequest, ServiceResponse]("profileMetricsEndpoint")(_ => dump) |+| endpoint[ServiceRequest, ServiceResponse]("/profile", MatchesServiceRequest.fixedPath(Get))

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
