package org.validoc.sample.domain

import org.validoc.utils.json.{FromJson, ToJson}


class DomainJson(implicit
                 val fromJsonForHomePageQuery: FromJson[HomePageQuery],
                 val toJsonForHomePage: ToJson[HomePageQuery],
                 val toJsonForMostPopualrQuery: ToJson[MostPopularQuery],
                 val fromJsonForMostPopular: FromJson[MostPopular]) {

}
