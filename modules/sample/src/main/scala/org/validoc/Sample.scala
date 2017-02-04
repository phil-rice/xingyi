//package org.validoc
//
//import java.util.concurrent.Executors
//
//import org.validoc.utils.concurrency.{Async, AsyncWithFailure}
//import org.validoc.utils.http._
//import org.validoc.utils.serviceBuilder.ServiceBuilderLanguage
//
//import scala.concurrent.{ExecutionContext, Future}
//
//
//
//trait Finatra extends ServiceBuilderLanguage {
//
//  trait FinatraHttpReq
//
//  trait FinatraHttpRes
//
//  type HttpReq = FinatraHttpReq
//  type HttpRes = FinatraHttpRes
//  type M[T] = Future[T]
//  type HttpF = Throwable
//
//
//
//}
//
//
//object SampleServices extends Finatra {
//  val ecForHttpRequests = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(1000))
//
//  override implicit def asyncWithFailureForHttp: AsyncWithFailure[Future, Throwable] = Async.AsyncWithFailureForFuture(ecForHttpRequests)
//
//  override implicit def toServiceResponse: ToServiceResponse[_root_.org.validoc.SampleServices.FinatraHttpRes] = ???
//}
//
//case class ContentfulQuery(bypassCache: Boolean)
//
//object ContentfulQuery {
//
//  import SampleServices._
//
//  implicit object ToHttpReqForContentfulQuery extends ToHttpReq[ContentfulQuery] {
//    override def toHttpRequest(req: ContentfulQuery): _root_.org.validoc.SampleServices.FinatraHttpReq = ???
//
//    override def toHost(req: ContentfulQuery): String = ???
//
//    override def toUri(req: ContentfulQuery): String = ???
//
//    override def toAcceptHeader(req: ContentfulQuery): String = ???
//  }
//
//}
//
//case class Contentful(contentfulData: String)
//
//object Sample extends App {
//
//  import SampleServices._
//
//  val fnordHttpService = rawHttpClient
//  val vogueHttpService = rawHttpClient
//  val contentfulHttpService = rawHttpClient
//
//  val responseProcessor = new ResponseProcessorForOption[ContentfulQuery, Contentful, HttpF](x => Contentful(x))
//
//  val contentfulService = contentfulHttpService.objectService("contentful", ResponseProcessor.optionalParsed[ContentfulQuery, Contentful, Throwable](x => Contentful(x)))
//
//}
