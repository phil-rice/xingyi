package one.xingyi.scriptWebsite

import one.xingyi.core.http._
import one.xingyi.core.strings.Strings
import one.xingyi.scriptExample.createdCode.{Person, PersonLine12Ops}
import one.xingyi.core.language.AnyLanguage._
import one.xingyi.core.monad.Monad

import scala.language.higherKinds

case class PersonAddressRequest(name: String)
object PersonAddressRequest {
  implicit def fromServiceRequest[M[_] : Monad]: FromServiceRequest[M, PersonAddressRequest] = {
    sr => PersonAddressRequest(Strings.lastSection("/")(sr.path.path)).liftM[M]
  }
  implicit val toServiceRequest: ToServiceRequest[PersonAddressRequest] = par =>
    ServiceRequest(Method("get"), Uri(s"http://localhost:9001/person/${par.name}"))
}
case class PersonAddressResponse(name: String, line1: String, line2: String)

object PersonAddressResponse {
  implicit def fromXingYi: FromXingYi[PersonAddressRequest, PersonAddressResponse] = {
    implicit xingYi =>
      req =>
        json =>
          val person = xingYi.parse[Person](json)
          val ops = new PersonLine12Ops()
          PersonAddressResponse(req.name, ops.line1Lens(person), ops.line2Lens(person))
  }
  implicit def toServiceResponse: ToServiceResponse[PersonAddressRequest, PersonAddressResponse] = cdreq => cdres =>
    ServiceResponse(Status(200), Body(cdres.toString), ContentType("text/html"))


}