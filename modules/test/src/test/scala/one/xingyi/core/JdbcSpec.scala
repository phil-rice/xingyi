package one.xingyi.core

import java.sql.ResultSet

import one.xingyi.core.cddlegacy.{LegacyData, LegacyResult}
import one.xingyi.core.closable.{ClosableM, SimpleClosable}
import one.xingyi.core.jdbc.Jdbc
import org.apache.commons.dbcp2.BasicDataSource
import org.scalatest.BeforeAndAfterAll
import one.xingyi.core.closable.ClosableLanguage._
import scala.language.higherKinds

class AbstractJdbcSpec[M[_] : ClosableM] extends UtilsSpec with BeforeAndAfterAll with Jdbc {

  val ds = new BasicDataSource
  ds.setDriverClassName("org.h2.Driver")
  ds.setUrl("jdbc:h2:~/test")
  ds.setUsername("sa")
  ds.setPassword("")

  override protected def afterAll(): Unit = {
    ds.close()
    super.afterAll()
  }

  behavior of "JDBC"

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
    val x = getValue("select * from testsource;")(toLegacyData) apply ds
    x shouldBe LegacyData(1, "sit1", "result1")
  }

  it should "batch things" in {
    setup(7)
    getValue("select count(*) from testsource")(rs => rs.getInt(1)) apply ds shouldBe 7
    def fn(legacyData: LegacyData[String, String]) = LegacyResult(legacyData.id, if (legacyData.id < 3) None else Some(s"id: ${legacyData.id}"))
    val x = process[M, LegacyData[String, String], LegacyResult](5)("select * from testsource", toLegacyData)(s"insert into testresult values (?,?)", lr => List(lr.id.toString, lr.failure.orNull)) _
    x(fn _)(ds).close()
    getValue("select count(*) from testresult")(rs => rs.getInt(1)) apply (ds) shouldBe 7
    val list = getList("select * from testresult")(toLegacyResult)apply (ds)
    list shouldBe List(LegacyResult(1,None), LegacyResult(2,None), LegacyResult(3,Some("id: 3")), LegacyResult(4,Some("id: 4")), LegacyResult(5,Some("id: 5")), LegacyResult(6,Some("id: 6")), LegacyResult(7,Some("id: 7")))


  }

}

class JdbcSpec extends AbstractJdbcSpec[SimpleClosable]
