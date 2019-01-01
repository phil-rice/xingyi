/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.objectify

import one.xingyi.core.http._
import one.xingyi.core.json._
import one.xingyi.core.language.Language._
import one.xingyi.core.logging.DetailedLogging
import one.xingyi.core.monad._
import one.xingyi.core.script._

import scala.language.higherKinds
import scala.reflect.ClassTag
import one.xingyi.core.language.AnyLanguage._
import one.xingyi.core.optics.Lens

case class EntityDetailsUrl[Req](url: Uri)

case class EntityDetailsRequest(bookmarkedUrl: String)

object EntityDetailsRequest {
  implicit def toServiceRequest: ToServiceRequest[EntityDetailsRequest] = edr => ServiceRequest(Method("get"), Uri(edr.bookmarkedUrl))
}

case class EntityDetailsResponse(urlPattern: String)

object EntityDetailsResponse extends JsonParserLanguage {
  implicit def fromServiceResponse[J](implicit jsonParser: JsonParser[J]): FromServiceResponse[EntityDetailsResponse] =
    sr => EntityDetailsResponse(jsonParser(sr.body.s) \ "url")


}

trait FromEntityDetailsResponse[Req] extends ((Req, ServerDomain) => EntityDetailsResponse => ServiceRequest)


trait XingyiKleisli[M[_], Fail] {
  protected implicit def monad: MonadCanFailWithException[M, Fail] with MonadWithState[M]

  protected implicit def failer: Failer[Fail]

  protected implicit def detailedLoggingForSR: DetailedLogging[ServiceResponse]


  def withXingyi[Req: ClassTag : DetailedLogging,
  Dom <: one.xingyi.core.script.Domain : ClassTag : DetailedLogging : DomainMaker,
  Ops <: IXingYiSharedOps[Lens, Dom] : ClassTag,
  Res](serverDomain: ServerDomain, fn: (Req, Dom, Ops) => Res)(http: ServiceRequest => M[ServiceResponse])
      (implicit entityDetailsUrl: EntityDetailsUrl[Dom],
       fromServiceResponseForEntityDetails: FromServiceResponse[EntityDetailsResponse],
       fromEntityDetailsResponse: FromEntityDetailsResponse[Req],
       xingYiLoader: IXingYiLoader,
       interfaceHeaders: IXingYiHeaderFor[Ops]): Req => M[Res] = {

    req =>
      RecordedCall.default.remove()
      for {
        serviceDiscoveryProducedServiceRequest <- http(ServiceRequest(Method("get"), entityDetailsUrl.url)).
          map(fromServiceResponseForEntityDetails andThen fromEntityDetailsResponse(req, serverDomain))
        withCorrectHeaders = serviceDiscoveryProducedServiceRequest.addHeader("accept", DomainDefn.accepts(interfaceHeaders()))
        codeBody <- http(withCorrectHeaders).map(ServiceResponse.serviceResponseToXingYiCodeAndBody)
        (code, body) = codeBody
        _ = println(s"code is $code")
        xingyi <- http(ServiceRequest(Method("get"), Uri(code))).map(sr => xingYiLoader(sr.body.s))
      } yield {
        println("About to parse")
        val dom = xingyi.parse[Dom](body)
        val ops = implicitly[ClassTag[Ops]].runtimeClass.getConstructor(classOf[IXingYi]).newInstance(xingyi).asInstanceOf[Ops]
        fn(req, dom, ops)
      }
  }

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
        xingyi <- http(ServiceRequest(Method("get"), Uri(code))).map(sr => xingYiLoader(sr.body.s))
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
        xingyi <- http(ServiceRequest(Method("get"), Uri(code))).map(sr => xingYiLoader(sr.body.s))
      } yield {
        fromXingYi(xingyi)(req)(body)
      }

  }
}


case class ResultWithRecordedCalls[T](t: T, calls: Seq[RecordedCall])

object ResultWithRecordedCalls {
  implicit def toJsonLib[T](implicit tToJson: ToJsonLib[T], recordedCallsToJson: ToJsonLib[RecordedCall]): ToJsonLib[ResultWithRecordedCalls[T]] =
    rc => JsonObject("original" -> tToJson(rc.t), "recorded" -> JsonList(rc.calls.map(recordedCallsToJson)))

}

case class RecordedCall(req: ServiceRequest, res: ServiceResponse)

object RecordedCall extends JsonWriterLanguage {
  implicit val default: InheritableThreadLocal[Seq[RecordedCall]] = new InheritableThreadLocal[Seq[RecordedCall]] {
    override def initialValue(): Seq[RecordedCall] = Seq()
  }

  implicit def toJsonLib(implicit serviceRequestToJson: ToJsonLib[ServiceRequest], serviceResponseToJson: ToJsonLib[ServiceResponse]): ToJsonLib[RecordedCall] =
    rc => JsonObject("request" -> serviceRequestToJson(rc.req), "response" -> serviceResponseToJson(rc.res))

}

trait RecordCallsKleisli[M[_], Fail] {
  def recordCalls(service: ServiceRequest => M[ServiceResponse])(implicit monad: Monad[M], recordedCalls: InheritableThreadLocal[Seq[RecordedCall]]): ServiceRequest => M[ServiceResponse] =
    req => service(req).map { res => recordedCalls.set(recordedCalls.get :+ RecordedCall(req, res)); res }

}

trait AddRecordedCalls[R]

trait ObjectifyKleisli[M[_], Fail] {
  protected implicit def monad: MonadCanFail[M, Fail]

  protected implicit def failer: Failer[Fail]

  protected implicit def detailedLoggingForSR: DetailedLogging[ServiceResponse]

  def objectify[Req: ClassTag : DetailedLogging, Res: ClassTag](http: ServiceRequest => M[ServiceResponse])
                                                               (implicit toRequest: ToServiceRequest[Req],
                                                                categoriser: ResponseCategoriser[Req],
                                                                responseProcessor: ResponseParser[Req, Res]): Req => M[Res] =
    toRequest ~> http |=|+> categoriser.categorise[Fail] |=|> responseProcessor.parse[Fail]
}
