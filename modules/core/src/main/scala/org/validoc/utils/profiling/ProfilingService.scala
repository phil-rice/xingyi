package org.validoc.utils.profiling

import org.validoc.utils.Service
import org.validoc.utils.concurrency.Async
import org.validoc.utils.concurrency.Async._
import org.validoc.utils.service.{MakeServiceDescription, ServiceComposition}
import org.validoc.utils.time.{NanoTimeService, SystemClockNanoTimeService}

import scala.language.higherKinds
import scala.reflect.ClassTag
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


trait ProfilingServiceLanguage[M[_]] extends ServiceComposition[M] {
  def profile[Req: ClassTag, Res: ClassTag](implicit timeService: NanoTimeService, async: Async[M]): MakeServiceDescription[M, Req, Res, Req, Res] =
    service[Req, Res, Req, Res, ProfilingService[M, Req, Res]](new ProfilingService("someName", _))

}

class ProfilingService[M[_] : Async, Req, Res](val name: String, delegate: Req => M[Res], timeService: NanoTimeService = SystemClockNanoTimeService, val tryProfileData: TryProfileData = new TryProfileData)
  extends Service[M, Req, Res] with ProfileInfo {

  override def apply(request: Req): M[Res] = {
    val start = timeService()
    delegate(request).registerSideEffectWhenComplete(tryProfileData.event(timeService() - start))
  }
}
