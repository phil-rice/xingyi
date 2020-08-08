package one.xingyi.core.proveInProduction

import one.xingyi.core.language.MonadLanguage._
import one.xingyi.core.monad.MonadCanFail

import scala.language.higherKinds

trait TwoServiceFailer[M[_], Fail] {
  def nothingSelected: Fail

  def requiredServiceNotPresent(details: String): Fail
}

object TwoServiceFailer {
  implicit def twoServiceFailerDefault[M[_], Throwable]: TwoServiceFailer[M, Throwable] = new TwoServiceFailer[M, Throwable] {
    override def nothingSelected: Throwable = new RuntimeException("Nothing selected").asInstanceOf[Throwable]

    override def requiredServiceNotPresent(details: String) = new RuntimeException("Required service not present: " + details).asInstanceOf[Throwable]
  }
}

trait TwoServiceProcessor[M[_], Fail, Req, Res] {
  def selectAndTransformRequests: Req => M[(Option[Req], Option[Req])]

  def postProcessResults: TwoServiceMerger[M, Req, Res]
}

case class TwoService[M[_], Fail, Req, Res](
                                             twoServiceProcessor: TwoServiceProcessor[M, Fail, Req, Res],
                                             service1: Req => M[Res],
                                             service2: Req => M[Res]
                                           )(implicit monad: MonadCanFail[M, Fail], twoServiceFailer: TwoServiceFailer[M, Fail]) extends (Req => M[Res]) {
  def optionallyCallAndPrefixReq(service: Req => M[Res], optReq: Option[Req]): Option[(Req, M[Res])] = optReq.map(req => (req, service(req)))

  def apply(req: Req): M[Res] = {
    twoServiceProcessor.selectAndTransformRequests(req).flatMap { case (optReq1, optReq2) =>
      twoServiceProcessor.postProcessResults(req,
        optionallyCallAndPrefixReq(service1, optReq1),
        optionallyCallAndPrefixReq(service2, optReq2))
    }
  }
}