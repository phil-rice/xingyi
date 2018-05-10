package one.xingyi.sample

import one.xingyi.sample.domain._
import one.xingyi.core.json.{FromJson, ToJson}

case class JsonBundle(implicit
                      val toJsonForHomePage: ToJson[HomePage],
                      val toJsonForEnrichedMostPopular: ToJson[EnrichedMostPopular],
                      val fromJsonForMostPopular: FromJson[MostPopular],
                      val fromJsonForPromotion: FromJson[Promotion],
                      val fromJsonForProgramme: FromJson[Programme],
                      val fromJsonForProduction: FromJson[Production])
