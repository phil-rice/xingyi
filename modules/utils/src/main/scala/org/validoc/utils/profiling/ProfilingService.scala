package org.validoc.utils.profiling

import org.validoc.utils.Service
import org.validoc.utils.concurrency.Async
import org.validoc.utils.time.{NanoTimeService, SystemClockNanoTimeService}

import scala.util.{Failure, Success, Try}
import org.validoc.utils.concurrency.Async._


trait TryProfileData {
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

object TryProfileData extends TryProfileData

class ProfilingService[M[_] : Async, Req, Res](name: String, delegate: Req => M[Res], timeService: NanoTimeService=SystemClockNanoTimeService, tryProfileData: TryProfileData = TryProfileData)
  extends Service[M, Req, Res] {

  override def apply(request: Req): M[Res] = {
    val start = timeService()
    delegate(request).registerSideEffectWhenComplete(tryProfileData.event(timeService() - start))
  }
}
