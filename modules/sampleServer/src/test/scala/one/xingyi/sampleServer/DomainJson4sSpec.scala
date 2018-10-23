package one.xingyi.sampleServer

import one.xingyi.sample.domain.{HomePageSpec, MostPopularSpec, PromotionSpec}
import org.json4s.JsonAST.JValue
import one.xingyi.json4s.Json4sWriter._
import one.xingyi.json4s.Json4sParser._

class HomePageJson4sSpec extends HomePageSpec[JValue]

class MostPopularJson4sSpec extends MostPopularSpec[JValue]

class ProductionJson4sSpec extends PromotionSpec[JValue]