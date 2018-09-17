/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddexamples.qAndA

import java.util.ResourceBundle

import com.sun.net.httpserver.HttpExchange
import one.xingyi.cddmustache.{Mustache, MustacheWithTemplate}
import one.xingyi.core.json.JsonLanguage._
import one.xingyi.core.json.{JsonMaps, JsonObject, JsonString}
import one.xingyi.javaserver.{HttpUtils, PathAndHandler, SimpleHttpResponse, SimpleHttpServer}
import one.xingyi.json4s.Json4sWriter._
import org.json4s.JValue

import scala.util.Try

object FactsWebsite extends App with Question {
  implicit val resource = ResourceBundle.getBundle("message")
  implicit def template: MustacheWithTemplate[JValue] = Mustache.withTemplate("main.template.mustache") apply("question.mustache", "Coolness")
  val executor = HttpUtils.makeDefaultExecutor
  var theEntity = entity

  val decisionMaker = new MifidDecisionMaker().categoriser
  implicit val config = Config(balanceSheetThreshold = GBP(20000000), netTurnoverThreshold = GBP(400000000), ownFundsThreshold = GBP(2000000))

  //  decisionMaker.tools.trace("mifid")
  def html = {
    val dec = decision(theEntity) match {
      case None => JsonString("none")
      case Some((entity, name)) => JsonObject("name" -> name, "val" -> entity.toJson(List(), theEntity))
    }
    template.apply(JsonMaps(JsonObject("conclusion" -> Try(decisionMaker(EntityDetails(theEntity))).toString, "question" -> dec, "entity" -> blackboard.toJson(theEntity))))
  }
  new SimpleHttpServer(9000, executor,
    new PathAndHandler {
      override def path(): String = "/change"
      override def handle(httpExchange: HttpExchange): Unit = {
        try {
          val x = httpExchange.getRequestURI.getQuery.split("&").toList.flatMap(s => s.split('=').toList match {
            case left :: right :: _ => List(left -> right)
            case left :: _ => List(left -> "")
            case _ => List()
          })
          theEntity = blackboard.update(theEntity)(x)
          HttpUtils.process(httpExchange, () => new SimpleHttpResponse(200, "text/html", html))
        } catch {case e:Throwable => e.printStackTrace(); HttpUtils.process(httpExchange, () => new SimpleHttpResponse(200, "text/html", s"Error: $e"))}
      }
    },
    new PathAndHandler {
      override def path(): String = "/"
      override def handle(httpExchange: HttpExchange): Unit = {

        theEntity = entity

        HttpUtils.process(httpExchange, () => new SimpleHttpResponse(200, "text/html", html))
      }
    }
  ).start()

}
