package org.validoc.utils.profiling

import org.validoc.utils.Service
import org.validoc.utils.functions.Monad
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

trait ProfileInfo {
  def name: String

  def tryProfileData: TryProfileData
}


class ProfilingService[M[_] : Monad, Req, Res](val name: String, delegate: Req => M[Res],
                                                       val tryProfileData: TryProfileData = new TryProfileData)(implicit timeService: NanoTimeService = SystemClockNanoTimeService)
  extends Service[M, Req, Res] with ProfileInfo {

  override def apply(request: Req): M[Res] = {
    val start = timeService()
    delegate(request)


      //TODO .registerSideEffectWhenComplete(tryProfileData.event(timeService() - start))
  }
}

