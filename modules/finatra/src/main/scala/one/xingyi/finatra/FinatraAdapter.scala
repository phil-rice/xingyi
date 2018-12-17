/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.finatra

import com.twitter.finagle.http.{Method, Request, Response}
import com.twitter.finatra.http.response.ResponseBuilder
import com.twitter.finatra.utils.FuturePools
import com.twitter.util.{Await, FuturePool, Local, Return, Throw, Duration => TDuration, Future => TFuture}
import one.xingyi.core.http.{Header, _}
import one.xingyi.core.local.{Holder, SimpleLocalOps}
import one.xingyi.core.monad.{Async, LocalVariable, MonadCanFailWithException, MonadWithState}
import one.xingyi.core.strings.Strings

import scala.collection.concurrent.TrieMap
import scala.concurrent.duration.Duration
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.util.{Try => STry}

object AsyncForTwitterFuture {
  val localVariableMap = new TrieMap[LocalVariable[_], InheritableThreadLocal[_]]()

}
class AsyncForTwitterFuture(implicit futurePool: FuturePool) extends Async[TFuture] with MonadCanFailWithException[TFuture, Throwable] with MonadWithState[TFuture] {
  override def async[T](t: => T) = futurePool(t)
  override def respond[T](m: TFuture[T], fn: STry[T] => Unit) = m.respond(tryS => fn(tryS.asScala))
  override def await[T](m: TFuture[T]) = Await.result(m, TDuration.fromSeconds(5))
  override def delay[T](duration: Duration)(block: => TFuture[T]) = ???

  override def foldWithExceptionAndFail[T, T1](m: TFuture[T], fnE: Throwable => TFuture[T1], fnFailure: Throwable => TFuture[T1], fn: T => TFuture[T1]): TFuture[T1] = m.transform {
    case Return(t) => fn(t)
    case Throw(t) => fnE(t)
  }
  override def exception[T](t: Throwable) = TFuture.exception(t)
  override def recover[T](m: TFuture[T], fn: Throwable => TFuture[T]): TFuture[T] = m.rescue { case e: Throwable => fn(e) }
  override def flatMapEither[T, T1](m: TFuture[T], fn: Either[Throwable, T] => TFuture[T1]): TFuture[T1] = {
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


  def getLocal[V](localVariable: LocalVariable[V]) =
    AsyncForTwitterFuture.localVariableMap.getOrElseUpdate(localVariable,
      new InheritableThreadLocal[Seq[V]]() {override def initialValue(): Seq[V] = Seq()}).asInstanceOf[InheritableThreadLocal[Seq[V]]]


  override def clear: TFuture[Unit] = {
    AsyncForTwitterFuture.localVariableMap.values.foreach(_.remove())
    TFuture.value()
  }

  override def putInto[V, T](localVariable: LocalVariable[V], t: V)(m: TFuture[T]): TFuture[T] = m.map { x =>
    val local: InheritableThreadLocal[Seq[V]] = getLocal(localVariable)
    val oldSeq = local.get
    local.set(oldSeq ++ Seq(t))
    x
  }
  override def mapState[V, T, T1](m: TFuture[T], localVariable: LocalVariable[V], fn: Seq[V] => T1): TFuture[T1] = m.map { x => fn(getLocal(localVariable).get) }
  override def mapWith[V, T, T1](m: TFuture[T], localVariable: LocalVariable[V], fn: (T, Seq[V]) => T1): TFuture[T1] = m.map { x =>
    val local = getLocal(localVariable)
    val seq = local.get()
    fn(x, seq)
  }
}

object FinatraAdapter {
  def liftEndpoint(response: ResponseBuilder, fn: ServiceRequest => TFuture[Option[ServiceResponse]])(implicit toServiceRequest: ToServiceRequest[Request]): Request => TFuture[Response] = { request: Request =>
    val serviceRequest = implicitly[ToServiceRequest[Request]] apply (request)
    val result = fn(serviceRequest)
    result.map {
      case Some(serRes) => response.status(serRes.status.code).body(serRes.body.s).contentType(serviceRequest.contentType.map(_.value).getOrElse("text/html"))
      case None => response.status(404).body(s"Endpoint  ${serviceRequest.method}  ${serviceRequest.uri} not found").contentType("text/html")
    }
  }
}
object FinatraImplicits {

  val localHolder = new Holder[Local] {
    override def makeHolder[V: ClassTag]: Local[V] = new Local()
    override def getValueOutOfHolder[V](holder: Local[V]): Option[V] = holder()
    override def putValueInHolder[V](v: Option[V])(holder: Local[V]): Unit = holder.set(v)
  }

  implicit val localOps = new SimpleLocalOps[TFuture, Local](localHolder)
  object ImplicitsForTest {
    implicit val futurePool = FuturePools.fixedPool("Future pool for tests", 20)
    implicit val asyncForTwitter = new AsyncForTwitterFuture

  }

  implicit def ToServiceResponseForFinatraResponse[Req]: ToServiceResponse[Req, Response] = sr => response =>
    ServiceResponse(status = Status(response.status.code), body = Body(response.contentString), headers = response.headerMap.map { case (n, v) => Header(n, v) }.toList)


  implicit object ToServiceRequest extends ToServiceRequest[Request] {
    override def apply(request: Request): ServiceRequest = {
      val headers = request.headerMap.map { case (n, v) => Header(n, v) }.toList
      val string = Strings.toOption(request.contentString)
      ServiceRequest(Get, uri = Uri(request.uri), headers = headers, body = string.map(Body.apply))
    }
  }

  implicit object FromServiceRequestForFinatraRequest extends FromServiceRequest[TFuture, Request] {
    override def apply(serviceRequest: ServiceRequest) = {
      val request = Request(Method(serviceRequest.method.toString.toUpperCase), serviceRequest.uri.asUriString)
      serviceRequest.headers.foreach { h => request.headerMap.add(h.name, h.value) }
      serviceRequest.body.foreach(body => request.setContentString(body.s))
      TFuture.value(request)
    }
  }
}
