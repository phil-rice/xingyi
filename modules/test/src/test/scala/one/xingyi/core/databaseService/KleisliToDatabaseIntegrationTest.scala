package one.xingyi.core.databaseService

import javax.sql.DataSource
import one.xingyi.core.monad.{IdentityMonad, Monad}
import org.apache.commons.dbcp2.BasicDataSource
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Success

class KleisliToDatabaseIntegrationTest extends FlatSpec with Matchers {
  lazy val dataSource: DataSource = {
    val bds = new BasicDataSource
    bds.setDriverClassName("org.h2.Driver")
    bds.setUrl("jdbc:h2:mem:")
    bds.setUsername("sa")
    bds.setPassword("")
    bds.setMaxConnLifetimeMillis(4000)
    bds
  }
  implicit def strToSqlAndParams(s: String): SqlAndParams = SqlAndParams(s, Seq())
  val nameToSql: Map[String, SqlAndParams] = Map(
    "drop" -> "DROP TABLE IF EXISTS TEST",
    "create" -> "CREATE TABLE IF NOT EXISTS TEST(ID INT PRIMARY KEY, NAME VARCHAR(255)) ",
    "insert" -> SqlAndParams("insert into TEST(id, name) values (?,?)", Seq("id", "name")),
    "size" -> "select count(1) from TEST",
    "select" -> "select * from TEST"
  )

  def setup[M[_] : Monad](block: (QueryRequest => M[QueryResponse], UpdateRequest => M[UpdateResponse]) => Unit): Unit = {
    val update = StoredProcedureKleisliFactory.update(dataSource, nameToSql)
    val query = StoredProcedureKleisliFactory.query[M](dataSource, nameToSql)
    block(query, update)
  }

  behavior of "kleisli integration tests"

  it should "drop  table, create table, insert values, read values" in {
    val zeroRecord = Success(UpdateResponse(0))
    val oneRecord = Success(UpdateResponse(1))
    setup[IdentityMonad] { (query, update) =>
      update(UpdateRequest("drop", Map())).value shouldBe zeroRecord
      update(UpdateRequest("create", Map())).value shouldBe zeroRecord
      update(UpdateRequest("insert", Map("id" -> "1", "name" -> "one"))).value shouldBe oneRecord
      update(UpdateRequest("insert", Map("id" -> "2", "name" -> "two"))).value shouldBe oneRecord
      query(QueryRequest("select", Map())).value shouldBe Success(QueryResponse(QueryResults(List("ID", "NAME"), List(List("1", "one"), List("2", "two")))))
    }

  }


}
