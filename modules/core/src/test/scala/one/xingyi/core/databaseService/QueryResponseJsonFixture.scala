package one.xingyi.core.databaseService

trait QueryResponseJsonFixture{

  val queryResponse = QueryResponse(QueryResults(List("a", "b"), List(List("1", "2"), List("11", "12"))))
  val responseJson = """{"names":["a","b"],"values":[["1","2"],["11","12"]]}"""

}