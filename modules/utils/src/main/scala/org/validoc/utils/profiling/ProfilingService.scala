package org.validoc.utils.profiling

import org.validoc.utils.Futurable
import org.validoc.utils.service.WrappingService
import org.validoc.utils.time.NanoTimeService


class ProfilingService[M[_] : Futurable, Req, Res](name: String, delegate: Req => M[Res], timeService: NanoTimeService, override val priority: Int) extends WrappingService[M, Req, Res](name, delegate) {

  import Futurable._

  val succeededData = new PageQueryMeteredData
  val failedData = new PageQueryMeteredData

  def clearData = {
    succeededData.clearData
    failedData.clearData
  }

  override def apply(request: Req): M[Res] = {
    val start = timeService()

    def addTime(data: PageQueryMeteredData)(ignore: Any) = {
      val duration = timeService() - start
      data.event(duration)
    }

    delegate(request).onComplete(addTime(succeededData), addTime(failedData))
  }
}
