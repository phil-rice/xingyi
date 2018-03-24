package org.validoc.tagless

import org.validoc.utils.aggregate.{Enricher, HasChildren}
import org.validoc.utils.cache.{CachableKey, ShouldCacheResult, ShouldUseCache}
import org.validoc.utils.endpoint.MatchesServiceRequest
import org.validoc.utils.functions.{Monad, MonadWithException}
import org.validoc.utils.http._
import org.validoc.utils.logging.{DetailedLogging, SummaryLogging}
import org.validoc.utils.metrics.ReportData
import org.validoc.utils.profiling.{ProfileAs, ProfileKleisli, TryProfileData}
import org.validoc.utils.retry.{NeedsRetry, RetryConfig}
import org.validoc.utils.strings.{IndentAnd, Strings}
import org.validoc.utils.time.NanoTimeService

import scala.collection.concurrent.TrieMap
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.language.implicitConversions

trait Profiled {
  def name: String
  def description: String
  def tryProfileData: TryProfileData
  def allChildren: Seq[Profiled]
}

class Profile2[M[_] : MonadWithException] {
  type Kleisli[Req, Res] = Req => M[Res]

  //TODO Need to add children to this so that we can do an HTML dump of it.
  // But this looks like it will work!
  //So this wraps every node in out tree, and we should be able to see the web page with the profiles of everything!
  case class ProfilingWrapper[Req: ClassTag, Res: ClassTag](name: String, description: String, kleisli: Req => M[Res], children: ProfilingWrapper[_, _]*)(implicit nanoTimeService: NanoTimeService) extends PartialFunction[Req, M[Res]] with Kleisli[Req, Res] with Profiled {
    val tryProfileData = new TryProfileData
    val profiledKleisli = ProfileKleisli(tryProfileData)(kleisli)

    override def apply(v1: Req) = profiledKleisli(v1)
    val allChildren: Seq[Profiled] = children.flatMap(child => Seq(child) ++ child.allChildren)
    def indents[T](fn: ProfilingWrapper[_, _] => T): IndentAnd[T] = {
      IndentAnd.merge(fn(this), children.map(_.indents(fn)): _*)
    }

    import org.validoc.utils.reflection.ClassTags._

    override def toString = s"ProfilingWrapper  ${children.size} ${allChildren.size} ($name, $description) [${nameOf[Req]}, ${nameOf[Res]}]  }"
    override def isDefinedAt(x: Req) = kleisli match {
      case pf: PartialFunction[Req, _] => pf.isDefinedAt(x)
      case _ => true
    }
  }

  import org.validoc.utils.language.Language._

  def endpointForProfiler[Req, Res](name: String, interpreter: TaglessLanguage[ProfilingWrapper, M], profilingWrapper: ProfilingWrapper[Req, Res]) =
    interpreter.endpoint[ServiceRequest, ServiceResponse](name, MatchesServiceRequest.fixedPath(Get))(
      ProfilingWrapper("endpointForProfiler", name, { serviceRequest: ServiceRequest =>
        val indentAndString = profilingWrapper.indents(pf => (s"${pf.name} ${pf.description}", pf.tryProfileData.toShortString))

        val result = "<table><tr>" + indentAndString.invertIndent.toString("</tr><tr>", { case (depth, (l, r)) => s"<td>${Strings.indent("&nbsp;&nbsp;&nbsp;", depth)}$l</td><td>$r</td>" }) + "</tr></table>"
        ServiceResponse(result).liftM[M]
      }))

  def Language(interpreter: TaglessLanguage[Kleisli, M]) = new TransformTaglessLanguage[Kleisli, ProfilingWrapper, M](new WrapperTransformer[Kleisli, ProfilingWrapper, M] {
    override def apply[Req: ClassTag, Res: ClassTag](name: String, description: String, fn: TaglessLanguage[Kleisli, M] => Kleisli[Req, Res], children: ProfilingWrapper[_, _]*): ProfilingWrapper[Req, Res] =
      ProfilingWrapper(name, description, fn(interpreter), children: _*)

  })

  def makeSystemAndProfileEndpoint[X](interpreter: TaglessLanguage[Kleisli, M],
                                      name: String,
                                      maker: TaglessLanguage[ProfilingWrapper, M] => X,
                                      endPoints: X => ProfilingWrapper[ServiceRequest, Option[ServiceResponse]]): (X, ProfilingWrapper[ServiceRequest, Option[ServiceResponse]]) = {
    val profiledAllLanguage: TaglessLanguage[ProfilingWrapper, M] = Language(interpreter)
    val result: X = maker(profiledAllLanguage)
    (result, endpointForProfiler(name, profiledAllLanguage, endPoints(result)))
  }

}

abstract class WrapperTransformer[Wrapper[_, _], Wrapper2[_, _], M[_]](implicit evidence: Wrapper2[_, _] <:< Wrapper[_, _]) {
  def apply[Req: ClassTag, Res: ClassTag](name: String, description: String, fn: TaglessLanguage[Wrapper, M] => Wrapper[Req, Res], children: Wrapper2[_, _]*): Wrapper2[Req, Res]

}

import org.validoc.utils.reflection.ClassTags._

class TransformTaglessLanguage[Wrapper[_, _], Wrapper2[_, _], M[_]](transform: WrapperTransformer[Wrapper, Wrapper2, M])(implicit evidence: Wrapper2[_, _] <:< Wrapper[_, _]) extends TaglessLanguage[Wrapper2, M] {
  //TODO I have no idea how to avoid these conversions. It's safe to do because of the evidence provided, but not nice
  implicit def toWrapper[Req, Res](w2: Wrapper2[Req, Res]): Wrapper[Req, Res] = w2.asInstanceOf[Wrapper[Req, Res]]
  implicit def toSeqWrapper[Req, Res](w2: Seq[Wrapper2[Req, Res]]): Seq[Wrapper[Req, Res]] = w2.asInstanceOf[Seq[Wrapper[Req, Res]]]

  override def http(name: ServiceName): Wrapper2[ServiceRequest, ServiceResponse] =
    transform("http", name.name, _.http(name))
  override def function[Req: ClassTag, Res: ClassTag](name: String)(fn: Req => Res): Wrapper2[Req, Res] =
    transform("function", name, _.function(name)(fn))
  override def objectify[Req: ClassTag : DetailedLogging, Res: ClassTag](http: Wrapper2[ServiceRequest, ServiceResponse])(implicit toRequest: ToServiceRequest[Req], categoriser: ResponseCategoriser[Req], responseProcessor: ResponseParser[Req, Res]): Wrapper2[Req, Res] =
    transform[Req, Res]("objectify", "", _.objectify[Req, Res](http), http)
  override def andAfter[Req: ClassTag, Mid: ClassTag, Res2: ClassTag](raw: Wrapper2[Req, Mid], fn: Mid => Res2): Wrapper2[Req, Res2] =
    transform[Req, Res2]("andAfter", "", _.andAfter[Req, Mid, Res2](raw, fn), raw)
  override def andAfterK[Req: ClassTag, Mid: ClassTag, Res2: ClassTag](raw: Wrapper2[Req, Mid], fn: Mid => M[Res2]): Wrapper2[Req, Res2] =
    transform[Req, Res2]("andAfterK", "", _.andAfterK[Req, Mid, Res2](raw, fn), raw)
  override def logging[Req: ClassTag : DetailedLogging : SummaryLogging, Res: ClassTag : DetailedLogging : SummaryLogging](messagePrefix: String)(raw: Wrapper2[Req, Res]): Wrapper2[Req, Res] =
    transform[Req, Res]("logging", messagePrefix, _.logging[Req, Res](messagePrefix)(raw), raw)
  override def metrics[Req: ClassTag, Res: ClassTag : ReportData](prefix: String)(raw: Wrapper2[Req, Res]): Wrapper2[Req, Res] =
    transform("metrics", prefix, _.metrics(prefix)(raw), raw)
  override def cache[Req: ClassTag : CachableKey : ShouldUseCache, Res: ClassTag : ShouldCacheResult](name: String)(raw: Wrapper2[Req, Res]): Wrapper2[Req, Res] =
    transform("cache", name, _.cache(name)(raw))
  override def retry[Req: ClassTag, Res: ClassTag : NeedsRetry](retryConfig: RetryConfig)(raw: Wrapper2[Req, Res]): Wrapper2[Req, Res] =
    transform("retry", retryConfig.toString, _.retry(retryConfig)(raw), raw)
  override def profile[Req: ClassTag, Res: ClassTag:ProfileAs](profileData: TryProfileData)(raw: Wrapper2[Req, Res]): Wrapper2[Req, Res] =
    transform("profile", "", _.profile(profileData)(raw), raw)
  override def endpoint[Req: ClassTag, Res: ClassTag](normalisedPath: String, matchesServiceRequest: MatchesServiceRequest)(raw: Wrapper2[Req, Res])(implicit fromServiceRequest: FromServiceRequest[M, Req], toServiceResponse: ToServiceResponse[Res]): Wrapper2[ServiceRequest, Option[ServiceResponse]] =
    transform("endpoint", normalisedPath, _.endpoint[Req, Res](normalisedPath, matchesServiceRequest)(raw), raw)
  override def chain(endpoints: Wrapper2[ServiceRequest, Option[ServiceResponse]]*): Wrapper2[ServiceRequest, Option[ServiceResponse]] =
    transform("chain", "", _.chain(endpoints: _*), endpoints: _*)
  override def enrichPrim[ReqP: ClassTag, ResP, ReqC, ResC, ResE: ClassTag](parent: Wrapper2[ReqP, ResP], child: Wrapper2[ReqC, ResC])(implicit findChildIds: HasChildren[ResP, ReqC], enricher: Enricher[ReqP, ResP, ReqC, ResC, ResE]): Wrapper2[ReqP, ResE] =
    transform("enrich", "", _.enrichPrim[ReqP, ResP, ReqC, ResC, ResE](parent, child), parent, child)
  override def merge2Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2](firstService: Wrapper2[Req1, Res1], secondService: Wrapper2[Req2, Res2], merger: (ReqM, Res1, Res2) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2): Wrapper2[ReqM, ResM] =
    transform("merge2", "", _.merge2Prim[ReqM, ResM, Req1, Res1, Req2, Res2](firstService, secondService, merger), firstService, secondService)

  override def merge3Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2, Req3, Res3](firstService: Wrapper2[Req1, Res1], secondService: Wrapper2[Req2, Res2], thirdService: Wrapper2[Req3, Res3], merger: (ReqM, Res1, Res2, Res3) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3): Wrapper2[ReqM, ResM] =
    transform("merge3", "", _.merge3Prim[ReqM, ResM, Req1, Res1, Req2, Res2, Req3, Res3](firstService, secondService, thirdService, merger), firstService, secondService, thirdService)
  override def merge4Prim[ReqM: ClassTag, ResM: ClassTag, Req1, Res1, Req2, Res2, Req3, Res3, Req4, Res4](firstService: Wrapper2[Req1, Res1], secondService: Wrapper2[Req2, Res2], thirdService: Wrapper2[Req3, Res3], fourthService: Wrapper2[Req4, Res4], merger: (ReqM, Res1, Res2, Res3, Res4) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3, reqMtoReq4: ReqM => Req4): Wrapper2[ReqM, ResM] =
    transform("merge4", "", _.merge4Prim[ReqM, ResM, Req1, Res1, Req2, Res2, Req3, Res3, Req4, Res4](firstService, secondService, thirdService, fourthService, merger), firstService, secondService, thirdService, fourthService)
}


class DelegatesTaglessLanguage[Wrapper[_, _], M[_]](interpreter: TaglessLanguage[Wrapper, M]) extends TaglessLanguage[Wrapper, M] {
  override def endpoint[Req: ClassTag, Res: ClassTag](normalisedPath: String, matchesServiceRequest: MatchesServiceRequest)(raw: Wrapper[Req, Res])(implicit fromServiceRequest: FromServiceRequest[M, Req], toServiceResponse: ToServiceResponse[Res]) =
    interpreter.endpoint[Req, Res](normalisedPath, matchesServiceRequest)(raw)
  override def chain(endpoints: Wrapper[ServiceRequest, Option[ServiceResponse]]*) = interpreter.chain(endpoints: _*)
  override def http(name: ServiceName) = interpreter.http(name)
  override def andAfter[Req: ClassTag, Mid: ClassTag, Res2: ClassTag](raw: Wrapper[Req, Mid], fn: Mid => Res2): Wrapper[Req, Res2] =
    interpreter.andAfter(raw, fn)
  override def andAfterK[Req: ClassTag, Mid: ClassTag, Res2: ClassTag](raw: Wrapper[Req, Mid], fn: Mid => M[Res2]): Wrapper[Req, Res2] =
    interpreter.andAfterK(raw, fn)
  override def objectify[Req: ClassTag : DetailedLogging, Res: ClassTag](http: Wrapper[ServiceRequest, ServiceResponse])(implicit toRequest: ToServiceRequest[Req], categoriser: ResponseCategoriser[Req], responseProcessor: ResponseParser[Req, Res]): Wrapper[Req, Res] =
    interpreter.objectify(http)
  override def logging[Req: ClassTag : DetailedLogging : SummaryLogging, Res: ClassTag : DetailedLogging : SummaryLogging](messagePrefix: String)(raw: Wrapper[Req, Res]) =
    interpreter.logging(messagePrefix)(raw)
  override def metrics[Req: ClassTag, Res: ClassTag : ReportData](prefix: String)(raw: Wrapper[Req, Res]) =
    interpreter.metrics(prefix)(raw)
  override def cache[Req: ClassTag : CachableKey : ShouldUseCache, Res: ClassTag : ShouldCacheResult](name: String)(raw: Wrapper[Req, Res]) =
    interpreter.cache(name)(raw)
  override def retry[Req: ClassTag, Res: ClassTag : NeedsRetry](retryConfig: RetryConfig)(raw: Wrapper[Req, Res]) =
    interpreter.retry(retryConfig)(raw)
  override def profile[Req: ClassTag, Res: ClassTag:ProfileAs](profileData: TryProfileData)(raw: Wrapper[Req, Res]) =
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

class ProfileEachEndpointLanguage[Wrapper[_, _], M[_] : Monad](interpreter: TaglessLanguage[Wrapper, M]) extends DelegatesTaglessLanguage[Wrapper, M](interpreter) {
  //TODO So here we have an interesting example of where a State monad would actually help. I think it would mean we didn't have mutable code and the interpreter wasn't stateless
  //This code is remarkably easier though...
  val map = TrieMap[String, TryProfileData]()
  override def endpoint[Req: ClassTag, Res: ClassTag](normalisedPath: String, matchesServiceRequest: MatchesServiceRequest)(raw: Wrapper[Req, Res])(implicit fromServiceRequest: FromServiceRequest[M, Req], toServiceResponse: ToServiceResponse[Res]) = {
    val data = map.getOrElseUpdate(normalisedPath + matchesServiceRequest, new TryProfileData)
    interpreter.endpoint(normalisedPath, matchesServiceRequest)(raw |+| profile(data))
  }

  def dump = ServiceResponse(Status(200), Body(map.map { case (k, v) => (f"$k%-40s" + " " + v.toShortString).replaceAll(" ", "&nbsp;") }.mkString("<br />")), ContentType("text/html "))

  def profileMetricsEndpoint(profileEndpoint: String) = function[ServiceRequest, ServiceResponse]("profileMetricsEndpoint")(_ => dump) |+| endpoint[ServiceRequest, ServiceResponse](profileEndpoint, MatchesServiceRequest.fixedPath(Get))

}

