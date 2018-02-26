package org.validoc.utils.endpoint

import org.validoc.utils.Service
import org.validoc.utils.concurrency.Async
import org.validoc.utils.http._

import scala.language.higherKinds


trait EndPointInfo[M[_]] extends Service[M, ServiceRequest, ServiceResponse] {
  def path: String
}


abstract class EndPointService[M[_] : Async, Req: FromServiceRequest, Res: ToServiceResponse](val path: String, delegate: Service[M, Req, Res])
  extends EndPointInfo[M] {

//  implicitly[FromServiceRequest[Req]] andThen implicitly[ToServiceResponse[Res]] >>> toServiceResponse

}

