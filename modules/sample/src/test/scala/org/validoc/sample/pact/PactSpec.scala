package org.validoc.sample.pact

import com.itv.scalapact.ScalaPactForger._
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll}
import org.validoc.sample.domain.SampleJsonsForCompilation
import org.validoc.sample.{PromotionServiceNames, PromotionSetup}
import org.validoc.utils.http.{FromServiceRequest, ServiceName, ToServiceResponse}
import org.validoc.utils.{Closable, UtilsSpec}

import scala.language.higherKinds
import scala.reflect.ClassTag
//
//class HttpServiceHolder[M[_] : AsyncLanguage, HttpReq, HttpRes, S <: HttpReq => M[HttpRes] : Closable](makeServiceOnLocalHost: Int => S) extends (HttpReq => M[HttpRes]) {
//  def clear = service = None
//
//  var service: Option[S] = None
//
//  def setPort(port: Int) = {
//    service match {
//      case Some(s) => implicitly[Closable[S]].close(s)
//      case _ =>
//    }
//    service = Some(makeServiceOnLocalHost(port))
//    //      Http.client.newService(s"localhost:$port"))
//  }
//
//  override def apply(v1: HttpReq) = service.get(v1)
//}
//
//abstract class PactSpec[
//M[_] : AsyncLanguage,
//HttpReq: FromServiceRequest : CachableKey : ClassTag,
//HttpRes: ToServiceResponse : CachableResult : ClassTag,
//S <: HttpReq => M[HttpRes] : Closable,
//Server: Closable](implicit serverBuilder: ServerBuilder[M, Server]) extends UtilsSpec with SampleJsonsForCompilation with PromotionServiceNames with BeforeAndAfter with BeforeAndAfterAll {
//
//  type HSH = HttpServiceHolder[M, HttpReq, HttpRes, S]
//
//
//  val endPoint = ServiceName("EndPoint")
//  val config: Map[ServiceName, HSH] = Map[ServiceName, HSH](
//    programmeAndProductionServiceName -> new HSH(makeServiceOnLocalHost),
//    promotionServiceName -> new HSH(makeServiceOnLocalHost),
//    mostPopularServiceName -> new HSH(makeServiceOnLocalHost),
//    endPoint -> new HSH(makeServiceOnLocalHost)
//  )
//
//  def makeServiceOnLocalHost(port: Int): S
//
//  implicit def makeHttpService = MakeHttpService[M, HttpReq, HttpRes](config)
//
//  def makePromotion(): PromotionSetup[M, HttpReq, HttpRes]
//
//  def serviceTrees: List[ServiceTree[M, _, _, ServiceDescription]]
//
//  val promotionSetup = makePromotion()
//
//  val server = implicitly[ServerBuilder[M, Server]].apply(9000, List(homePageEndPoint, enrichedMostPopularEndPoint))
//
//
//
//  after {
//    config.values.foreach(_.clear)
//    serviceTrees.foreach(_.findAllServices[CachingService[M, _, _]].foreach(_.clearCache))
//  }
//
//  override def afterAll(): Unit = {
//    super.afterAll()
//    implicitly[Closable[Server]].close(server)
//  }
//
//  def makePact(provider: ServiceName, interaction: ScalaPactInteraction*)(block: => Unit) = {
//    interaction.foldLeft(forgePact.between("HomePage").and(provider.name))((acc, i) => acc.addInteraction(i)).runConsumerTest {
//      mockConfig =>
//        config(provider).setPort(mockConfig.port)
//        block
//    }
//  }
//
//  def programInteraction(i: Int) = {
//    interaction
//      .description(s"Request for programme $i")
//      .given(s"programme id [$i] exists")
//      .uponReceiving(s"/programme/$i")
//      .willRespondWith(200,
//        s"""someProgramme${
//          i
//        }Info""")
//  }
//
//  def productionInteraction(i: Int) = {
//    interaction
//      .description(s"Request for production $i")
//      .given(s"production id [$i] exists")
//      .uponReceiving(s"/production/$i")
//      .willRespondWith(200,
//        s"""someProduction${
//          i
//        }Info""")
//  }
//
//  def mostPopular(i: Int*) = {
//    val s = i.mkString("[", ",", "]")
//    interaction
//      .description(s"Request for most popular $s")
//      .given(s"most popular is $s")
//      .uponReceiving("/mostpopular")
//      //.uponReceiving("/token/id/1")
//      .willRespondWith(200, s)
//  }
//
//  def promotion(i: Int*) = {
//    val s = i.mkString("[", ",", "]")
//    interaction
//      .description(s"Request for promotions $s")
//      .given(s"promotions is $s")
//      .uponReceiving("/promotion")
//      //.uponReceiving("/token/id/1")
//      .willRespondWith(200, s)
//  }
//
//}
//
