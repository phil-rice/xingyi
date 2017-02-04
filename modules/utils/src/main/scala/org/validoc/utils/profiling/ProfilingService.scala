package org.validoc.utils.profiling

import org.validoc.utils.Service
import org.validoc.utils.concurrency.Async
import org.validoc.utils.time.NanoTimeService


class ProfilingService[M[_] : Async, F, Req, Res](name: String, delegate: Req => M[Res], timeService: NanoTimeService)
  extends Service[M, Req, Res] {

  import org.validoc.utils.concurrency.Async._

  val succeededData = new ProfileData
  val failedData = new ProfileData

  def clearData = {
    succeededData.clearData
    failedData.clearData
  }

  override def apply(request: Req): M[Res] = {
    val start = timeService()

    def addTime(data: ProfileData)(ignore: Any) = {
      val duration = timeService() - start
      data.event(duration)
    }

    delegate(request).registerSideEffectWhenComplete(_.fold(addTime(succeededData), addTime(failedData)))
  }
}
