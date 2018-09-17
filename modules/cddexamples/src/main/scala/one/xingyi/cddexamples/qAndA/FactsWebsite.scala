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
