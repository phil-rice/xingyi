package one.xingyi.core.databaseService

trait DatabaseRequest {
  def name: String
  def map: Map[String, String]
}

object DatabaseRequest {
  def needException(msg: String) = new RuntimeException(s"body of request must include a Json object. $msg")
}