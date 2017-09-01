package org.validoc.utils.profiling

import org.validoc.utils.Service
import org.validoc.utils.caching.{CachableKey, CachableResult, CachingService, StaleCacheStrategy}
import org.validoc.utils.concurrency.Async
import org.validoc.utils.concurrency.Async._
import org.validoc.utils.map.MapSizeStrategy
import org.validoc.utils.serviceTree.ServiceLanguageExtension
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


class ProfilingService[M[_] : Async, Req, Res](val name: String, delegate: Req => M[Res],
                                               val tryProfileData: TryProfileData = new TryProfileData)(implicit timeService: NanoTimeService = SystemClockNanoTimeService)
  extends Service[M, Req, Res] with ProfileInfo {

  override def apply(request: Req): M[Res] = {
    val start = timeService()
    delegate(request).registerSideEffectWhenComplete(tryProfileData.event(timeService() - start))
  }
}

trait ProfilingServiceLanguageExtension[M[_]] extends ServiceLanguageExtension[M] {
  def profile[Req: ClassTag : CachableKey, Res: ClassTag : CachableResult](name: String, tryProfileData: TryProfileData = new TryProfileData)
                                                                          (implicit timeService: NanoTimeService): ServiceDelegator[Req, Res] = { childTree =>
    delegate(s"Profile($name)", childTree, new ProfilingService[M, Req, Res](name, _, tryProfileData))
  }
}