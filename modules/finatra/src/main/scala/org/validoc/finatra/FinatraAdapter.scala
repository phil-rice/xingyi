package org.validoc.finatra

import com.twitter.finagle.http.{Method, Request, Response}
import com.twitter.finatra.utils.FuturePools
import com.twitter.util.{Await, FuturePool, Return, Throw, Duration => TDuration, Future => TFuture, Try => TTry}
import org.validoc.language.AggregatedServicesInterpreter
import org.validoc.utils.caching.CachableResult
import org.validoc.utils.concurrency.Async
import org.validoc.utils.http._
import org.validoc.utils.metrics.NullPutMetrics
import org.validoc.utils.service.ServerContext
import org.validoc.utils.success.SucceededFromFn
import org.validoc.utils.time.SystemClockNanoTimeService

import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

object FinatraAdapter extends FinatraAdapter()(FuturePools.fixedPool("pool", 20))

class FinatraAdapter(implicit val futurePool: FuturePool) {

  implicit def asyncForTwitterFuture(implicit futurePool: FuturePool) = new Async[TFuture] {
    override def async[T](t: => T): TFuture[T] = futurePool(t)

    override def delay(duration: FiniteDuration): TFuture[Unit] = TFuture(())

    override def transform[T1, T2](mt: TFuture[T1], fn: (Try[T1]) => TFuture[T2]): TFuture[T2] = mt.transform {
      _ match {
        case Return(t) => fn(Success(t))
        case Throw(t) => fn(Failure(t))
      }
    }

    override def registerSideEffectWhenComplete[T](m: TFuture[T], sideEffect: Try[T] => _): TFuture[T] =
      m.onSuccess(t => sideEffect(Success(t))).onFailure(t => sideEffect(Failure(t)))

    override def await[T](mt: TFuture[T], duration: Duration): T = Await.result(mt, TDuration.fromNanoseconds(duration.toNanos))

    override def lift[T](t: => T): TFuture[T] = TFuture(t)

    /** This may just throw the exception if it's not meaningfull to lift it. Meaningful monads include Try, Future ... */
    override def liftTry[T](tryT: Try[T]): TFuture[T] =
      tryT match {
        case Success(t) => TFuture(t)
        case Failure(t) => TFuture.exception(t)
      }

    override def map[T, T2](m: TFuture[T], fn: (T) => T2): TFuture[T2] = m.map(fn)

    override def flatMap[T, T2](m: TFuture[T], fn: (T) => TFuture[T2]): TFuture[T2] = m.flatMap(fn)
  }

  implicit object ToServiceResponseForFinatraResponse extends ToServiceResponse[Response] {
    override def apply(response: Response): ServiceResponse = ServiceResponse(Status(response.statusCode), Body(response.contentString), ContentType(response.mediaType.getOrElse("")))
  }

  implicit object ToServiceRequest extends ToServiceRequest[Request] {
    override def apply(request: Request): ServiceRequest = ServiceRequest(Get, Uri(request.path), request.headerMap.get("Accept").map(AcceptHeader(_)))
  }

  implicit object FromServiceRequestForFinatraRequest extends FromServiceRequest[Request] {
    override def apply(serviceRequest: ServiceRequest): Request = Request(Method(serviceRequest.method.toString.toUpperCase), serviceRequest.uri.asUriString)
  }

  implicit  object CachableResultForResponse extends CachableResult[Response] {
    override def shouldCacheStrategy(req: Try[Response]): Boolean = req.isSuccess
  }

  implicit val serverContext: ServerContext[Request, Response] = {


    implicit val nanoTimeService = SystemClockNanoTimeService
    implicit val serviceData = new AggregatedServicesInterpreter[TFuture, Request, Response]
    implicit val putMetrics = NullPutMetrics
    implicit val succeeded = new SucceededFromFn[Response](_.getStatusCode() / 100 == 2)
    new ServerContext[Request, Response]
  }
}
