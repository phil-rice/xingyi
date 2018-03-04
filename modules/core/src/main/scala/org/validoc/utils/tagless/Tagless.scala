package org.validoc.utils.tagless

import org.validoc.utils.cache.{Cachable, ShouldCache}
import org.validoc.utils.endpoint.MatchesServiceRequest
import org.validoc.utils.http._
import org.validoc.utils.logging._
import org.validoc.utils.metrics.ReportData
import org.validoc.utils.profiling.TryProfileData
import org.validoc.utils.retry.{NeedsRetry, RetryConfig}

import scala.language.higherKinds
import scala.reflect.ClassTag

trait FindReq[Main, Req] extends (Main => Req)

object FindReq {
  implicit def identityFindReq[X] = new FindReq[X, X] {
    override def apply(v1: X): X = v1
  }
}

trait HasChildren[Main, Children] extends (Main => Seq[Children])

trait Enricher[Req, Parent, ChildId, Child, Res] extends ((Req, Parent, Seq[(ChildId, Child)]) => Res)

trait MetricsLanguageBase[Fail] {
  type RD[T] = ReportData[Fail, T]
}

trait SimpleLanguage[Wrapper[_, _]] {
  def function[Req, Res](name: String)(fn: Req => Res): Wrapper[Req, Res]
}
trait TaglessLanguage[EndpointWrapper[_, _], Wrapper[_, _], M[_], Fail] extends HttpLanguage[Wrapper, M, Fail]
  with SimpleLanguage[Wrapper] with NonfunctionalLanguage[Wrapper, Fail] with ComposeLanguage[Wrapper] with EndpointLanguage[EndpointWrapper, Wrapper, M, Fail]

sealed trait DebugInfo[M[_], Req, Res]
case class HasDebugInfo[M[_], Req, Res](implicit val fromServiceRequest: FromServiceRequest[M, Req], toServiceResponse: ToServiceResponse[Res]) extends DebugInfo[M, Req, Res]
case class NoDebugInfo[M[_], Req, Res](comment: String = "No debug info available") extends DebugInfo[M, Req, Res]

object DebugInfo {
  implicit def default[M[_], Req, Res] = new NoDebugInfo[M, Req, Res]
}


trait HttpLanguage[Wrapper[_, _], M[_], Fail] extends NonfunctionalLanguage[Wrapper, Fail] {
  def http(name: ServiceName): Wrapper[ServiceRequest, ServiceResponse]
  def objectify[Req: ClassTag, Res: ClassTag](http: Wrapper[ServiceRequest, ServiceResponse])(implicit toRequest: ToServiceRequest[Req], categoriser: ResponseCategoriser[M, Req], responseParser: ResponseParser[Fail, Req, Res]): Wrapper[Req, Res]
  //  def endpoint[Req: ClassTag:EndpointPath, Res: ClassTag:ToServiceResponse]()
}

trait NonfunctionalLanguage[Wrapper[_, _], Fail] extends MetricsLanguageBase[Fail] {
  def logging[Req: ClassTag : DetailedLogging : SummaryLogging, Res: ClassTag : DetailedLogging : SummaryLogging](messagePrefix: String)(raw: Wrapper[Req, Res]): Wrapper[Req, Res]

  def metrics[Req: ClassTag, Res: ClassTag : RD](prefix: String)(raw: Wrapper[Req, Res]): Wrapper[Req, Res]
  def cache[Req: ClassTag : Cachable : ShouldCache, Res: ClassTag](name: String)(raw: Wrapper[Req, Res]): Wrapper[Req, Res]
  def retry[Req: ClassTag, Res: ClassTag](retryConfig: RetryConfig)(raw: Wrapper[Req, Res])(implicit retry: NeedsRetry[Fail, Res]): Wrapper[Req, Res]
  def profile[Req: ClassTag, Res: ClassTag](profileData: TryProfileData)(raw: Wrapper[Req, Res]): Wrapper[Req, Res]

  implicit class ComposeWrapperPimper[RawReq, RawRes](wrapper: Wrapper[RawReq, RawRes]) {
    def |+|[Req, Res](fn: Wrapper[RawReq, RawRes] => Wrapper[Req, Res]): Wrapper[Req, Res] = fn(wrapper)
  }

  implicit class OtherPimper[Req, Res, NewReq, NewRes](wrapperFn: Wrapper[Req, Res] => Wrapper[NewReq, NewRes]) {
    def <+>[EvenNewerReq, EvenNewerRes](fn: Wrapper[NewReq, NewRes] => Wrapper[EvenNewerReq, EvenNewerRes]): Wrapper[Req, Res] => Wrapper[EvenNewerReq, EvenNewerRes] = wrapperFn andThen fn
  }

}

trait EndpointLanguage[EndpointWrapper[_, _], Wrapper[_, _], M[_], Fail] {
  def endpoint[Req: ClassTag, Res: ClassTag](normalisedPath: String, matchesServiceRequest: MatchesServiceRequest)(raw: Wrapper[Req, Res])(implicit fromServiceRequest: FromServiceRequest[M, Req], toServiceResponse: ToServiceResponse[Res]): EndpointWrapper[Req, Res]
  def chain(endpoints: EndpointWrapper[_, _]*): Wrapper[ServiceRequest, ServiceResponse]

  implicit class ComposeWrapper2Pimper[RawReq, RawRes](wrapper: Wrapper[RawReq, RawRes]) {
    def |++|[Req, Res](fn: Wrapper[RawReq, RawRes] => EndpointWrapper[Req, Res]): EndpointWrapper[Req, Res] = fn(wrapper)

  }

  implicit class OtherPimper2[Req, Res, NewReq, NewRes](wrapperFn: Wrapper[NewReq, NewRes] => EndpointWrapper[Req, Res]) {
    def <+>(fn: Wrapper[NewReq, NewReq] => Wrapper[NewReq, NewRes]): Wrapper[NewReq, NewReq] => EndpointWrapper[Req, Res] = fn andThen wrapperFn
    def <++>[EvenNewerReq, EvenNewerRes](fn: Wrapper[EvenNewerReq, EvenNewerRes] => Wrapper[NewReq, NewRes]): Wrapper[EvenNewerReq, EvenNewerRes] => EndpointWrapper[Req, Res] = fn andThen wrapperFn
    def <++>(wrapper: Wrapper[NewReq, NewRes]): EndpointWrapper[Req, Res] = wrapperFn(wrapper)
  }

  implicit class ComposeEndpointWrapperPimper[RawReq, RawRes](wrapper: EndpointWrapper[RawReq, RawRes]) {
    def |+++|[Req, Res](fn: EndpointWrapper[RawReq, RawRes] => Wrapper[Req, Res]): Wrapper[Req, Res] = fn(wrapper)
  }

}

trait ComposeLanguage[Wrapper[_, _]] {

  case class enrich[ReqP, ResP](parent: Wrapper[ReqP, ResP]) {

    case class withChild[ReqC, ResC](child: Wrapper[ReqC, ResC])(implicit findChildIds: HasChildren[ResP, ReqC]) {
      def mergeInto[ResE](implicit enricher: Enricher[ReqP, ResP, ReqC, ResC, ResE]): Wrapper[ReqP, ResE] = enrichPrim(parent, child)
    }

  }

  protected class Merge[Req1, Res1](firstService: Wrapper[Req1, Res1]) {

    case class and[Req2, Res2](secondService: Wrapper[Req2, Res2]) {
      def into[ReqM, ResM](merger: (ReqM, Res1, Res2) => ResM)(implicit findReq1: FindReq[ReqM, Req1], findReq2: FindReq[ReqM, Req2]): Wrapper[ReqM, ResM] =
        merge2Prim(firstService, secondService, merger)

      case class and[Req3, Res3](thirdService: Wrapper[Req3, Res3]) {
        def into[ReqM, ResM](merger: (ReqM, Res1, Res2, Res3) => ResM)(implicit findReq1: FindReq[ReqM, Req1], findReq2: FindReq[ReqM, Req2], findReq3: FindReq[ReqM, Req3]): Wrapper[ReqM, ResM] =
          merge3Prim(firstService, secondService, thirdService, merger)

        case class and[Req4, Res4](fourthService: Wrapper[Req4, Res4]) {
          def into[ReqM, ResM](merger: (ReqM, Res1, Res2, Res3, Res4) => ResM)(implicit findReq1: FindReq[ReqM, Req1], findReq2: FindReq[ReqM, Req2], findReq3: FindReq[ReqM, Req3], findReq4: FindReq[ReqM, Req4]): Wrapper[ReqM, ResM] = merge4Prim(firstService, secondService, thirdService, fourthService, merger)
        }

      }

    }

  }

  def merge[Req1, Res1](firstService: Wrapper[Req1, Res1]) = new Merge[Req1, Res1](firstService)

  def enrichPrim[ReqP, ResP, ReqC, ResC, ResE](parent: Wrapper[ReqP, ResP], child: Wrapper[ReqC, ResC])(implicit findChildIds: HasChildren[ResP, ReqC], enricher: Enricher[ReqP, ResP, ReqC, ResC, ResE]): Wrapper[ReqP, ResE]

  def merge2Prim[ReqM, ResM, Req1, Res1, Req2, Res2](firstService: Wrapper[Req1, Res1], secondService: Wrapper[Req2, Res2], merger: (ReqM, Res1, Res2) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2): Wrapper[ReqM, ResM]

  def merge3Prim[ReqM, ResM, Req1, Res1, Req2, Res2, Req3, Res3](firstService: Wrapper[Req1, Res1], secondService: Wrapper[Req2, Res2], thirdService: Wrapper[Req3, Res3], merger: (ReqM, Res1, Res2, Res3) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3): Wrapper[ReqM, ResM]

  def merge4Prim[ReqM, ResM, Req1, Res1, Req2, Res2, Req3, Res3, Req4, Res4](firstService: Wrapper[Req1, Res1], secondService: Wrapper[Req2, Res2], thirdService: Wrapper[Req3, Res3], fourthService: Wrapper[Req4, Res4], merger: (ReqM, Res1, Res2, Res3, Res4) => ResM)(implicit reqMtoReq1: ReqM => Req1, reqMtoReq2: ReqM => Req2, reqMtoReq3: ReqM => Req3, reqMtoReq4: ReqM => Req4): Wrapper[ReqM, ResM]
}





