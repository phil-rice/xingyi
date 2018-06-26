/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.sample.pact

import scala.language.higherKinds
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
