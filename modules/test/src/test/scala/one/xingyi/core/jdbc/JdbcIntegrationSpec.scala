/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.jdbc

import java.sql.ResultSet

import javax.sql.DataSource
import one.xingyi.core.cddlegacy.{LegacyData, LegacyResult}
import one.xingyi.core.closable.ClosableLanguage._
import one.xingyi.core.closable.{ClosableM, SimpleClosable}
import org.apache.commons.dbcp2.BasicDataSource

import scala.language.higherKinds

trait BasicDataSourceFixture extends DatabaseSourceFixture[BasicDataSource] {
  def makeDataSource(): BasicDataSource = {
    val bds= new BasicDataSource
    bds.setDriverClassName("org.h2.Driver")
    bds.setUrl("jdbc:h2:~/test")
    bds.setUsername("sa")
    bds.setPassword("")
    bds.setMaxConnLifetimeMillis(4000)
    bds
  }

  override def closeDataSource(ds: BasicDataSource): Unit = ds.close()
  //
  //  override protected def afterAll(): Unit = {
  //    ds.close()
  //    super.afterAll()
  //  }

}
abstract class AbstractJdbcIntegrationSpec[M[_] : ClosableM] extends BasicDataSourceFixture with Jdbc {

  behavior of "JDBC"

  val jdbcOps: JdbcOps[DataSource] = implicitly[JdbcOps[DataSource]]
  import jdbcOps._

  def toLegacyData(rs: ResultSet) = LegacyData(rs.getInt("id"), rs.getString("situation"), rs.getString("result"))
  def toLegacyResult(rs: ResultSet) = LegacyResult(rs.getInt("id"), Option(rs.getString("result")))

  def setup(i: Int) = {
    executeSql("drop table testsource if exists") apply ds
    executeSql("drop table testresult if exists") apply ds
    executeSql("create table testsource (id INT, situation VARCHAR(255), result VARCHAR(255));") apply ds
    executeSql("create table testresult (id INT, result VARCHAR(255));") apply ds
    1 to i foreach { i => executeSql(s"""insert into  testsource (id , situation , result ) values ($i, 'sit$i', 'result$i');""") apply ds }
  }


  it should
  "drop create  tables" in {
    setup(1)
    val x = getValue("select * from testsource;", toLegacyData) apply ds
    x shouldBe LegacyData(1, "sit1", "result1")
  }

  it should "batch things" in {
    setup(7)
    getValue("select count(*) from testsource", rs => rs.getInt(1)) apply ds shouldBe 7
    def fn(legacyData: LegacyData[String, String]) = LegacyResult(legacyData.id, if (legacyData.id < 3) None else Some(s"id: ${legacyData.id}"))
    val x = process[M, LegacyData[String, String], LegacyResult](5)("select * from testsource", toLegacyData)(s"insert into testresult values (?,?)", lr => List(lr.id.toString, lr.failure.orNull)) _
    x(fn _)(ds).close()
    getValue("select count(*) from testresult", rs => rs.getInt(1)) apply (ds) shouldBe 7
    val list = getList("select * from testresult")(toLegacyResult) apply (ds)
    list shouldBe List(LegacyResult(1, None), LegacyResult(2, None), LegacyResult(3, Some("id: 3")), LegacyResult(4, Some("id: 4")), LegacyResult(5, Some("id: 5")), LegacyResult(6, Some("id: 6")), LegacyResult(7, Some("id: 7")))


  }

}

class FastOrmSpec extends AbstractFastOrmSpec[SimpleClosable, BasicDataSource] with BasicDataSourceFixture

class JdbcIntegrationSpec extends AbstractJdbcIntegrationSpec[SimpleClosable]
