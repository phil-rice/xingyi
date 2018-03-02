package org.validoc.utils.domain

import org.mockito.Mockito._
import org.validoc.utils.UtilsSpec
import org.validoc.utils.http._
import org.validoc.utils.parser.{Parser, ParserFinder}
import org.validoc.utils.reflection.ClassTags

import scala.reflect.ClassTag

abstract class DomainCompanionQuerySpec[Req <: BypassCache : ClassTag] extends UtilsSpec {

  behavior of s"Domain companion ${ClassTags.nameOf[Req]}"

  //  def defaultContentType = ContentType("application/json")
  //
  //  implicit def fromServiceResponse(implicit parserFinder: ParserFinder[T]) = new FromServiceResponse[T] {
  //    override def apply(serviceResponse: ServiceResponse): T =
  //      parserFinder.find(defaultContentType).valueOrException(serviceResponse.body.s)
  //  }
  //  implicit def shouldCache = new ShouldCache[T] {
  //    override def apply(v1: T) = !v1.bypassCache
  val companion: DomainCompanionQuery[Req]

  val serviceRequest = ServiceRequest(Get, Uri("/someUri"), body = Some(Body("someBody")))

  it should "Have a implicit from service response if there is a parserFinder in scope" in {
//    implicit val parserfinder = mock[ParserFinder[Req]]
//    val parser = mock[Parser[Req]]
//    val req = mock[Req]
//    when(parserfinder.find(companion.defaultContentType)) thenReturn FoundResult(companion.defaultContentType, parser)
//    when(parser.apply("someBody")) thenReturn req
//    val x = fromServiceResponse
//    implicitly[FromServiceResponse[Req]].apply(serviceRequest) shouldBe req
  }

}

case class TestDomainQuery(name: String, bypassCache: Boolean) extends BypassCache

class DomainCompanionQueryForTestDomainQuerySpec extends DomainCompanionQuerySpec[TestDomainQuery] {
  override lazy val companion = new DomainCompanionQuery[TestDomainQuery] {}
}
