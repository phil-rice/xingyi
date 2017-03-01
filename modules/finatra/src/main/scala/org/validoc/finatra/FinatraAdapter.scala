package org.validoc.finatra

import com.twitter.finagle.http.{Request, Response}
import com.twitter.util.{Future, FuturePool}
import org.validoc.language.{MakeHttpService, ServiceInterpreters}
import org.validoc.utils.metrics.NullPutMetrics
import org.validoc.utils.success.SucceededFromFn
import org.validoc.utils.time.SystemClockNanoTimeService


trait FinatraAdapter extends FinatraPlayground{

  implicit val nanoTimeService = SystemClockNanoTimeService
  implicit val makeHttpService = MakeHttpService(MockFinatraService("mostPopular", "promotion", "programmeAndProductions"))
  implicit val futurePool = FuturePool.unboundedPool
  implicit val serviceData = new ServiceInterpreters.ServicesGroupedForAsync[Future, Request, Response]
  implicit val putMetrics = NullPutMetrics
  implicit val succeeded = new SucceededFromFn[Response](_.getStatusCode() / 100 == 2)

}
