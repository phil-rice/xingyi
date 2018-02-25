//package org.validoc.sample.pact
//
//import org.validoc.sample.PromotionServiceNames
//import org.validoc.sample.domain._
//import org.validoc.utils.Closable
//import org.validoc.utils.caching.{CachableKey, CachableResult}
//import org.validoc.utils.concurrency.AsyncLanguage
//import org.validoc.utils.concurrency.AsyncLanguage._
//import org.validoc.utils.http._
//import org.validoc.utils.server.ServerBuilder
//
//import scala.language.higherKinds
//import scala.reflect.ClassTag
//
//abstract class PromotionPactSpec[
//M[_] : AsyncLanguage,
//HttpReq: FromServiceRequest : CachableKey : ClassTag,
//HttpRes: ToServiceResponse : CachableResult : ClassTag,
//S <: HttpReq => M[HttpRes] : Closable, Server: Closable](implicit serverBuilder: ServerBuilder[M, Server]) extends PactSpec[M, HttpReq, HttpRes, S, Server] with PromotionServiceNames {
//
//  import promotionSetup._
//
//  behavior of "HomePage Service - Pact"
//
//  it should "return programmes when a programme id is sent" in {
//    makePact(programmeAndProductionServiceName, programInteraction(1)) {
//      new AsyncPimper(rawProgrammeService.service(ProgrammeId("1"))).await5s shouldBe Programme("someProgramme1Info")
//    }
//  }
//
//  it should "return productions when a production id is sent" in {
//    makePact(programmeAndProductionServiceName, productionInteraction(1)) {
//      rawProductionService.service(ProductionId("1")).await5s shouldBe Production("""someProduction1Info""")
//    }
//  }
//
//  it should "return the most popular list" in {
//    makePact(mostPopularServiceName, mostPopular(2, 3)) {
//      rawMostPopularService.service(MostPopularQuery).await5s shouldBe MostPopular(List(ProgrammeId("2"), ProgrammeId("3")))
//    }
//  }
//
//  it should "enrich the most popular with programme data " in {
//    makePact(mostPopularServiceName, mostPopular(2, 3)) {
//      makePact(programmeAndProductionServiceName, programInteraction(2), programInteraction(3)) {
//        enrichedMostPopular.service(MostPopularQuery).await5s shouldBe EnrichedMostPopular(List(Programme("someProgramme2Info"), Programme("someProgramme3Info")))
//      }
//    }
//  }
//
//
//  it should "return promotions" in {
//    makePact(promotionServiceName, promotion(2, 3)) {
//      rawPromotionService.service(PromotionQuery).await5s shouldBe Promotion(List(ProductionId("2"), ProductionId("3")))
//    }
//  }
//
//  it should "return enriched promotions" in {
//    makePact(promotionServiceName, promotion(2, 3)) {
//      makePact(programmeAndProductionServiceName, productionInteraction(2), productionInteraction(3)) {
//        enrichedPromotion.service(PromotionQuery).await5s shouldBe EnrichedPromotion(List(Production("someProduction2Info"), Production("someProduction3Info")))
//
//      }
//    }
//  }
//
//  it should "handle a home page query" in {
//    makePact(promotionServiceName, promotion(4, 5)) {
//      makePact(mostPopularServiceName, mostPopular(6, 7)) {
//        makePact(programmeAndProductionServiceName, productionInteraction(4), productionInteraction(5), programInteraction(6), programInteraction(7)) {
//          homePage.service(HomePageQuery).await5s shouldBe
//            HomePage(
//              EnrichedMostPopular(List(Programme("someProgramme6Info"), Programme("someProgramme7Info"))),
//              EnrichedPromotion(List(Production("someProduction4Info"), Production("someProduction5Info"))))
//        }
//      }
//    }
//  }
//
//  it should "handle the homepage endpoint" in {
//    makePact(promotionServiceName, promotion(4, 5)) {
//      makePact(mostPopularServiceName, mostPopular(6, 7)) {
//        makePact(programmeAndProductionServiceName, productionInteraction(4), productionInteraction(5), programInteraction(6), programInteraction(7)) {
//          val service = config(endPoint)
//          service.setPort(9000)
//          val httpReq = implicitly[FromServiceRequest[HttpReq]].apply(ServiceRequest(Get, Uri("/homepage")))
//          val httpRes = service(httpReq).await5s
//          val serviceResponse = implicitly[ToServiceResponse[HttpRes]].apply(httpRes)
//          serviceResponse.status.code shouldBe 200
//          serviceResponse.body.s shouldBe """HomePage(EnrichedMostPopular(List(Programme(someProgramme6Info), Programme(someProgramme7Info))),EnrichedPromotion(List(Production(someProduction4Info), Production(someProduction5Info))))"""
//        }
//      }
//    }
//  }
//}