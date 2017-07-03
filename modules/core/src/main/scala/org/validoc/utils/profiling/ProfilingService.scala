package org.validoc.utils.profiling

import org.validoc.utils.Service
import org.validoc.utils.concurrency.Async
import org.validoc.utils.concurrency.Async._
import org.validoc.utils.service.MakeServiceMakerForClass
import org.validoc.utils.time.{NanoTimeService, SystemClockNanoTimeService}

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

class TryProfileData {
  def clearData = {
    succeededData.clearData
    failedData.clearData
  }

  val succeededData = new ProfileData
  val failedData = new ProfileData

  def event(nanos: Long)(result: Try[_]): Unit = result match {
    case Success(_) => succeededData.event(nanos)
    case Failure(_) => failedData.event(nanos)
  }
}

trait ProfileOps {
  def name: String

  def tryProfileData: TryProfileData
}

object ProfilingService {
  implicit def makeProfileService[OldService <: Req => M[Res], M[_] : Async, Req, Res](implicit timeService: NanoTimeService) =
    new MakeServiceMakerForClass[OldService, ProfilingService[M, Req, Res]] {
      override def apply(delegate: OldService): ProfilingService[M, Req, Res] = new ProfilingService[M, Req, Res]("someName", delegate)
    }

}

class ProfilingService[M[_] : Async, Req, Res](val name: String, delegate: Req => M[Res], timeService: NanoTimeService = SystemClockNanoTimeService, val tryProfileData: TryProfileData = new TryProfileData)
  extends Service[M, Req, Res] with ProfileOps {

  override def apply(request: Req): M[Res] = {
    val start = timeService()
    delegate(request).registerSideEffectWhenComplete(tryProfileData.event(timeService() - start))
  }
}
