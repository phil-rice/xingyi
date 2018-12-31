package one.xingyi.scriptWebsite

import one.xingyi.core.http._
import one.xingyi.core.json._
import one.xingyi.core.language.AnyLanguage._
import one.xingyi.core.monad.Monad
import one.xingyi.core.objectify.{EntityDetailsUrl, FromEntityDetailsResponse}
import one.xingyi.core.service.html.ToHtml
import one.xingyi.core.strings.Strings
import one.xingyi.scriptExample.createdCode1.{Person, PersonLine12Ops}

import scala.language.higherKinds


case class PersonAddressRequest(name: String)

object PersonAddressRequest {
  implicit val entityDetails = EntityDetailsUrl[PersonAddressRequest](Uri("http://127.0.0.1:9001/person"))

  //TODO security flaw here. OK for now
  implicit def fromEntityDetailsRequest: FromEntityDetailsResponse[PersonAddressRequest] =
    (req, sd) => edr => ServiceRequest(Method("get"), Uri(edr.urlPattern.replace("<id>", req.name)), headers = List(Header("accept", sd.contentType)), body = None)

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

case class IndexPageRequest()

object IndexPageRequest {
  implicit def fromServiceRequest[M[_] : Monad]: FromServiceRequest[M, IndexPageRequest] =
    sr => IndexPageRequest().liftM[M]
}

case class IndexPageResponse()

object IndexPageResponse {
  implicit def toJson: ToJsonLib[IndexPageResponse] = _ => JsonString("")

  implicit def toServiceResponse(implicit toHtml: ToHtml[IndexPageResponse]): ToServiceResponse[IndexPageRequest, IndexPageResponse] =
    req => response => ServiceResponse(Status(200), Body(toHtml(response)), ContentType("text/html"))
}

case class DisplayEditPersonFormRequest(name: String)

object DisplayEditPersonFormRequest {
  implicit def fromServiceRequest[M[_] : Monad]: FromServiceRequest[M, DisplayEditPersonFormRequest] = sr =>
    DisplayEditPersonFormRequest(Strings.lastButOneSection("/")(sr.path.path)).liftM[M]

  implicit def entityUrl(implicit personEntityUrl: EntityDetailsUrl[PersonAddressRequest]): EntityDetailsUrl[DisplayEditPersonFormRequest] = EntityDetailsUrl(personEntityUrl.url)

  implicit def fromEntityDetailsResponse: FromEntityDetailsResponse[DisplayEditPersonFormRequest] = {
    (req, sd) => edr => ServiceRequest(Method("get"), Uri(edr.urlPattern.replace("<id>", req.name)), headers = List(Header("accept", sd.contentType)), body = None)

  }
}

case class DisplayEditPersonFormResponse(html: String)

object DisplayEditPersonFormResponse {

  implicit def toServiceResponse: ToServiceResponse[DisplayEditPersonFormRequest, DisplayEditPersonFormResponse] =
    req => res => ServiceResponse(Status(200), Body(res.html), ContentType("text/html"))

  implicit def fromXingYi: FromXingYi[DisplayEditPersonFormRequest, DisplayEditPersonFormResponse] = {
    implicit xingYi =>
      req =>
        json =>
          val person = xingYi.parse[Person](json)
          val html = xingYi.render("form", person)
          DisplayEditPersonFormResponse(html)

  }
}

case class EditPersonRequest(name: String, newLine1: String, newLine2: String)

object EditPersonRequest {
  implicit def fromServiceRequest[M[_] : Monad]: FromServiceRequest[M, EditPersonRequest] = { sr =>
    val params = Strings.paramsToMap(sr.body.map(_.s).getOrElse(""))
    EditPersonRequest(Strings.lastButOneSection("/")(sr.path.path), params("line1"), params("line2")).liftM[M]
  }

  implicit def entityUrl(implicit personEntityUrl: EntityDetailsUrl[PersonAddressRequest]): EntityDetailsUrl[EditPersonRequest] = EntityDetailsUrl(personEntityUrl.url)

  implicit def fromEntityDetailsResponse: FromEntityDetailsResponse[EditPersonRequest] = {
    (req, sd) => edr =>
      println(s"in fromEntityDetailsResponse. Req is $req sd is $sd and edr is $edr" )
      println(s"in fromEntityDetailsResponse. name is ${req.name}" )
      ServiceRequest(Method("get"), Uri(edr.urlPattern.replace("<id>", req.name)), headers = List(Header("accept", sd.contentType)), body = None)

  }
}

case class EditPersonResponse(json: String)

object EditPersonResponse {
  implicit def toServiceResponse: ToServiceResponse[EditPersonRequest, EditPersonResponse] = { req => res => ServiceResponse(Status(200), Body(res.json + "some content"), ContentType("application/json")) }

  implicit def fromXingYi: FromXingYi[EditPersonRequest, EditPersonResponse] = {
    implicit xingYi =>
      req =>
        json =>
          val person = xingYi.parse[Person](json)
          val ops = new PersonLine12Ops()
          val person2 = ops.line1Lens.set(person, req.newLine1)
          val person3 = ops.line2Lens.set(person2, req.newLine2)
          val pretty = xingYi.render("pretty", person3)
          EditPersonResponse(pretty)

  }

}
