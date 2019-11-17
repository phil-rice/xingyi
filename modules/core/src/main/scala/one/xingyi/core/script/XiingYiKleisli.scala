package one.xingyi.core.script
import one.xingyi.core.http._
import one.xingyi.core.json.{IXingYiHeaderFor, IXingYiSharedOps, JsonParser, JsonParserLanguage}
import one.xingyi.core.language.Language._
import one.xingyi.core.logging.DetailedLogging
import one.xingyi.core.monad.{MonadCanFailWithException, MonadWithState}
import one.xingyi.core.objectify.RecordedCall
import one.xingyi.core.optics.Lens

import scala.language.higherKinds
import scala.reflect.ClassTag

case class EntityDetailsUrl[Req](url: Uri)

case class EntityDetailsRequest(bookmarkedUrl: String)

object EntityDetailsRequest {
  implicit def toServiceRequest: ToServiceRequest[EntityDetailsRequest] = edr => ServiceRequest(Method("get"), Uri(edr.bookmarkedUrl))
}

case class EntityDetailsResponse(urlPattern: String)

object EntityDetailsResponse extends JsonParserLanguage {
  implicit def fromServiceResponse[J](implicit jsonParser: JsonParser[J]): FromServiceResponse[EntityDetailsResponse] =
    sr => EntityDetailsResponse(jsonParser(sr.body.asUtf) \ "url")


}

trait FromEntityDetailsResponse[Req] extends ((Req, ServerDomain) => EntityDetailsResponse => ServiceRequest)


trait XingyiKleisli[M[_], Fail] {
  protected implicit def monad: MonadCanFailWithException[M, Fail] with MonadWithState[M]

  protected implicit def failer: Failer[Fail]

  protected implicit def detailedLoggingForSR: DetailedLogging[ServiceResponse]


//  def withXingyi[Req: ClassTag : DetailedLogging,
//  Dom <: one.xingyi.core.script.Domain : ClassTag : DetailedLogging : DomainMaker,
//  Ops <: IXingYiSharedOps[Lens, Dom] : ClassTag,
//  Res](serverDomain: ServerDomain, fn: (Req, Dom, Ops) => Res)(http: ServiceRequest => M[ServiceResponse])
//      (implicit entityDetailsUrl: EntityDetailsUrl[Dom],
//       fromServiceResponseForEntityDetails: FromServiceResponse[EntityDetailsResponse],
//       fromEntityDetailsResponse: FromEntityDetailsResponse[Req],
//       xingYiLoader: IXingYiLoader,
//       interfaceHeaders: IXingYiHeaderFor[Ops]): Req => M[Res] = {
//
//    req =>
//      RecordedCall.default.remove()
//      for {
//        serviceDiscoveryProducedServiceRequest <- http(ServiceRequest(Method("get"), entityDetailsUrl.url)).
//          map(fromServiceResponseForEntityDetails andThen fromEntityDetailsResponse(req, serverDomain))
//        withCorrectHeaders = serviceDiscoveryProducedServiceRequest.addHeader("accept", DomainDefn.accepts(interfaceHeaders()))
//        codeBody <- http(withCorrectHeaders).map(ServiceResponse.serviceResponseToXingYiCodeAndBody)
//        (code, body) = codeBody
//        _ = println(s"code is $code")
//        xingyi <- http(ServiceRequest(Method("get"), Uri(code))).map(sr => xingYiLoader(sr.body.s))
//      } yield {
//        println("About to parse")
//        val dom = xingyi.parse[Dom](body)
//        val ops = implicitly[ClassTag[Ops]].runtimeClass.getConstructor(classOf[IXingYi]).newInstance(xingyi).asInstanceOf[Ops]
//        fn(req, dom, ops)
//      }
//  }
//
  def editXingYi[Req: ClassTag : DetailedLogging,
  Dom <: one.xingyi.core.script.Domain : ClassTag : DetailedLogging : DomainMaker,
  Ops <: IXingYiSharedOps[Lens, Dom] : ClassTag,
  Res](serverDomain: ServerDomain, fn: (Req, Ops) => Dom => Dom)(http: ServiceRequest => M[ServiceResponse])
      (implicit entityDetailsUrl: EntityDetailsUrl[Dom],
       fromServiceResponseForEntityDetails: FromServiceResponse[EntityDetailsResponse],
       fromEntityDetailsResponse: FromEntityDetailsResponse[Req], fromEditXingYi: FromEditXingYi[Req, Dom, Res],
       xingYiLoader: IXingYiLoader,
       interfaceHeaders: IXingYiHeaderFor[Ops]
      ): Req => M[Res] = {
    req =>
      RecordedCall.default.remove()
      for {
        serviceDiscoveryProducedServiceRequest <- http(ServiceRequest(Method("" + "get"), entityDetailsUrl.url)).map(fromServiceResponseForEntityDetails andThen fromEntityDetailsResponse(req, serverDomain))
        withCorrectHeaders = serviceDiscoveryProducedServiceRequest.addHeader("accept", DomainDefn.accepts(interfaceHeaders()))
        codeBody <- http(withCorrectHeaders).map(ServiceResponse.serviceResponseToXingYiCodeAndBody)
        (code, body) = codeBody
        xingyi <- http(ServiceRequest(Method("get"), Uri(code))).map(sr => xingYiLoader(sr.body.asUtf))
        dom = xingyi.parse[Dom](body)
        ops = implicitly[ClassTag[Ops]].runtimeClass.getConstructor(classOf[IXingYi]).newInstance(xingyi).asInstanceOf[Ops]
        modifiedDom = fn(req, ops)(dom)
        modifyServiceRequest = withCorrectHeaders.copy(method = Method("put"), body = Some(Body(xingyi.render("pretty", modifiedDom))))
        modifyResponse <- http(modifyServiceRequest)
      } yield {
        fromEditXingYi(req, modifiedDom, modifyResponse)
      }
  }

  def xingyify[Req: ClassTag : DetailedLogging, Res: ClassTag](serverDomain: ServerDomain)(http: ServiceRequest => M[ServiceResponse])
                                                              (implicit entityDetailsUrl: EntityDetailsUrl[Req],
                                                               fromServiceResponseForEntityDetails: FromServiceResponse[EntityDetailsResponse],
                                                               fromEntityDetailsResponse: FromEntityDetailsResponse[Req],
                                                               categoriser: ResponseCategoriser[Req],
                                                               xingYiLoader: IXingYiLoader,
                                                               fromXingYi: FromXingYi[Req, Res]): Req => M[Res] = {
    req =>
      RecordedCall.default.remove()
      for {
        serviceDiscoveryProducedServiceRequest <- http(ServiceRequest(Method("get"), entityDetailsUrl.url)).map(fromServiceResponseForEntityDetails andThen fromEntityDetailsResponse(req, serverDomain))
        codeBody <- http(serviceDiscoveryProducedServiceRequest).map(ServiceResponse.serviceResponseToXingYiCodeAndBody)
        (code, body) = codeBody
        xingyi <- http(ServiceRequest(Method("get"), Uri(code))).map(sr => xingYiLoader(sr.body.asUtf))
      } yield {
        fromXingYi(xingyi)(req)(body)
      }

  }
}

