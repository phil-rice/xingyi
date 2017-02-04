package org.validoc.utils.profiling

//
//class ProfilingService[M[_], F, Req, Res](name: String, delegate: Req => M[Res], timeService: NanoTimeService, override val priority: Int)
//                                         (implicit asyncWithFailure: AsyncWithFailure[M, F]) extends WrappingService[M, Req, Res](name, delegate) {
//
//  import org.validoc.utils.concurrency.Async._
//
//  val succeededData = new PageQueryMeteredData
//  val failedData = new PageQueryMeteredData
//
//  def clearData = {
//    succeededData.clearData
//    failedData.clearData
//  }
//
//  override def apply(request: Req): M[Res] = {
//    val start = timeService()
//
//    def addTime(data: PageQueryMeteredData)(ignore: Any) = {
//      val duration = timeService() - start
//      data.event(duration)
//    }
//
//    delegate(request).report[F](addTime(succeededData), addTime(failedData))
//  }
//}
