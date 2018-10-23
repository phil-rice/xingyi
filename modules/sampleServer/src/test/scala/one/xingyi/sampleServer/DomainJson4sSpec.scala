package one.xingyi.sampleServer

import one.xingyi.sample.domain._
import org.json4s.JsonAST.JValue
import one.xingyi.json4s.Json4sWriter._
import one.xingyi.json4s.Json4sParser._

class HomePageJson4sSpec extends HomePageSpec[JValue]

class MostPopularJson4sSpec extends MostPopularSpec[JValue]

class PromotionJson4sSpec extends PromotionSpec[JValue]

class ProductionJson4sSpec extends ProductionSpec[JValue]

class ProgrammeJson4sSpec extends ProgrammeSpec[JValue]