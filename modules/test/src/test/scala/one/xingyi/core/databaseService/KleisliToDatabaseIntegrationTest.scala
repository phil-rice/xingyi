/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.databaseService

import javax.sql.DataSource
import one.xingyi.core.UtilsSpec
import one.xingyi.core.monad.{IdentityMonad, Monad}
import org.apache.commons.dbcp2.BasicDataSource

import scala.language.{higherKinds, implicitConversions}
import scala.util.Success

class KleisliToDatabaseIntegrationTest extends UtilsSpec {
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
