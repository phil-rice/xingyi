package one.xingyi.finatra

import com.twitter.finagle.http.{Method, Request, Response}
import com.twitter.finatra.utils.FuturePools
import com.twitter.util.{Await, FuturePool, Local, Return, Throw, Duration => TDuration, Future => TFuture}
import one.xingyi.utils.functions.{Async, MonadCanFailWithException}
import one.xingyi.utils.http._
import one.xingyi.utils.local.{Holder, SimpleLocalOps}

import scala.concurrent.duration.Duration
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.util.{Try => STry}

class AsyncForTwitterFuture(implicit futurePool: FuturePool) extends Async[TFuture] with MonadCanFailWithException[TFuture, Throwable] {
  override def async[T](t: => T) = futurePool(t)
  override def respond[T](m: TFuture[T], fn: STry[T] => Unit) = m.respond(tryS => fn(tryS.asScala))
  override def await[T](m: TFuture[T]) = Await.result(m, TDuration.fromSeconds(5))
  override def delay[T](duration: Duration)(block: => TFuture[T]) = ???
  override def foldWithExceptionAndFail[T, T1](m: TFuture[T], fnE: Throwable => TFuture[T1], fnFailure: Throwable => TFuture[T1], fn: T => TFuture[T1]) = m.transform {
    case Return(t) => fn(t)
    case Throw(t) => fnE(t)
  }
  override def exception[T](t: Throwable) = TFuture.exception(t)
  override def recover[T](m: TFuture[T], fn: Throwable => TFuture[T]): TFuture[T] = m.rescue { case e: Throwable => fn(e) }
  override def mapEither[T, T1](m: TFuture[T], fn: Either[Throwable, T] => TFuture[T1]): TFuture[T1] = {
    m.transform {
      case Return(t) => fn(Right(t))
      case Throw(t) =>
        fn(Left(t))
    }
  }
  override def flatMap[T, T1](m: TFuture[T], fn: T => TFuture[T1]) = m.flatMap(fn)
  override def map[T, T1](m: TFuture[T], fn: T => T1) = m.map(fn)
  override def fail[T](f: Throwable) = TFuture.exception(f)
  override def liftM[T](t: T) = TFuture.value(t)
}

object FinatraImplicits {

  implicit def asyncForTwitter(implicit futurePool: FuturePool) = new AsyncForTwitterFuture
  val localHolder = new Holder[Local] {
    override def makeHolder[V: ClassTag]: Local[V] = new Local()
    override def getValueOutOfHolder[V](holder: Local[V]): Option[V] = holder()
    override def putValueInHolder[V](v: Option[V])(holder: Local[V]): Unit = holder.set(v)
  }

  implicit val localOps = new SimpleLocalOps[TFuture, Local](localHolder)
  object ImplicitsForTest {
    implicit val futurePool = FuturePools.fixedPool("Future pool for tests", 20)

  }

  implicit object ToServiceResponseForFinatraResponse extends ToServiceResponse[Response] {
    override def apply(response: Response): ServiceResponse = ServiceResponse(Status(response.statusCode), Body(response.contentString), ContentType(response.mediaType.getOrElse("")))
  }

  implicit object ToServiceRequest extends ToServiceRequest[Request] {
    override def apply(request: Request): ServiceRequest = ServiceRequest(Get, Uri(request.path), request.headerMap.get("Accept").map(AcceptHeader(_)))
  }

  implicit object FromServiceRequestForFinatraRequest extends FromServiceRequest[TFuture, Request] {
    override def apply(serviceRequest: ServiceRequest) = TFuture.value(Request(Method(serviceRequest.method.toString.toUpperCase), serviceRequest.uri.asUriString))
  }


}
