package org.validoc.finatra

import com.twitter.finagle.http
import com.twitter.finagle.http.{Request, Response}
import com.twitter.finatra.http.Controller
import org.validoc.utils.concurrency.Async
import org.validoc.utils.endpoint.EndPointService
import org.validoc.utils.http._
import org.validoc.utils.serviceTree.{ServiceDescription, ServiceTree}
import Async._
import com.twitter.util.Future

import scala.language.higherKinds

class EndpointController(serviceTrees: List[ServiceTree[Future, _, _, ServiceDescription]]) extends Controller {
  val endpoints: List[EndPointService[Future, _, _]] =
    serviceTrees.flatMap(serviceTree => serviceTree.findAll[EndPointService[Future, _, _]].map(_.service).collect { case e: EndPointService[Future, _, _] => e })

  val summary = endpoints.map(_.path).mkString(",")
  println(s"EndpointController: $summary")
  endpoints.foreach { endPoint =>
    println(s"Processing endpoint: $endPoint")
    get(endPoint.path) { request: Request =>
      println(s"On endpoint1: ${endPoint.path} ${request.uri} ${request.remoteAddress}")
      try {
        val serviceRequest = ServiceRequest(Get, Uri(request.uri), request.headerMap.get("Accept").map(AcceptHeader(_)))
        println(s"On endpoint2: ${endPoint.path} serviceRequest is $serviceRequest")
        val r = endPoint(serviceRequest).map { r: ServiceResponse =>
          response.status(http.Status(r.status.code)).body(r.body.s).contentType(r.contentType.s)
        }
        r.onFailure { case e => e.printStackTrace }
        r
      } catch {
        case e: Throwable => e.printStackTrace(); throw e
      }
    }
  }
}
