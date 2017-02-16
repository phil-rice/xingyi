package org.validoc.finatra

import com.twitter.finagle.http
import com.twitter.finagle.http.{Method, Request, Response}
import com.twitter.finatra.http.filters.{CommonFilters, LoggingMDCFilter, TraceIdMDCFilter}
import com.twitter.finatra.http.routing.HttpRouter
import com.twitter.finatra.http.{Controller, HttpServer}
import com.twitter.util.{Await, FuturePool, Return, Throw, Duration => TDuration, Future => TFuture, Try => TTry, _}
import org.validoc.utils.Service
import org.validoc.utils.concurrency.Async
import org.validoc.utils.http._
import org.validoc.utils.service.{ServiceData, ServiceInterpreters}
import org.validoc.utils.service.ServiceInterpreters.ServicesGroupedForAsync

import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.util.{Failure, Success, Try}

object FinatraPlayground {

  implicit object toServiceResponseForFinatraResponse extends ToServiceResponse[Response] {
    override def apply(response: Response): ServiceResponse =
      ServiceResponse(Status(response.statusCode), Body(response.contentString), ContentType(response.mediaType.getOrElse("")))
  }

  implicit def toServiceRequest(request: Request) = ServiceRequest(Get, Uri(request.path), request.headerMap.get("Accept").map(AcceptHeader(_)))

  implicit def toRequest(serviceRequest: ServiceRequest) = {
    Request(Method(serviceRequest.method.toString.toUpperCase), serviceRequest.uri.asUriString)
  }


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


}

class EndpointController(serviceData: ServiceData[TFuture, _, _]) extends Controller {
  println(s"EndpointController: $serviceData")
  println(s"EndpointController endpoints: ${serviceData.endPoints}")
  serviceData.endPoints.foreach { endPoint =>
    println(s"Processing endpoint: $endPoint")
    get(endPoint.path) { request: Request =>
      println(s"On endpoint: ${endPoint.path} ${request.uri} ${request.remoteAddress}")
      try {
        val serviceRequest = ServiceRequest(Get, Uri(Protocol("http"), HostName("unknown"), Port(80), Path(request.path)), request.headerMap.get("Accept").map(AcceptHeader(_)))
        println(s"On endpoint: ${endPoint.path} serviceRequest is $serviceRequest")
        endPoint(serviceRequest).map { r: ServiceResponse =>
          response.status(http.Status(r.status.code)).body(r.body.s).contentType(r.contentType.s)
        }
      } catch {
        case e: Throwable => e.printStackTrace(); throw e
      }
    }
  }
}

class StatusController(serviceData: ServiceData[TFuture, _, _]) extends Controller{
}

class PingController extends Controller {
  get("/ping") { request: Request => response.ok("pong").contentType("text/plain") }
}

class FinatraServer(port: Int, controllers: Controller*) extends HttpServer {
  override val modules = Seq()

  override def defaultHttpPort: Int = 9200

  override val defaultFinatraHttpPort: String = s":$port"

  override def configureHttp(router: HttpRouter): Unit = {
    val raw = router
      .filter[LoggingMDCFilter[Request, Response]]
      .filter[TraceIdMDCFilter[Request, Response]]
      .filter[CommonFilters]
    controllers.foldLeft(raw)((acc, c) => acc.add(c))
  }
}


