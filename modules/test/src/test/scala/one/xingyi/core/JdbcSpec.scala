package one.xingyi.core
import java.sql.{Connection, PreparedStatement, ResultSet, Statement}

import javax.sql.DataSource
import one.xingyi.core.closable.{ClosableLanguage, SimpleClosable}
import one.xingyi.core.jdbc.{Jdbc, JdbcOps}
import org.mockito.Mockito._

import scala.util.Random

class JdbcSpec extends UtilsSpec with ClosableFixture with Jdbc with ClosableLanguage {

  behavior of "jdbc.connection"

  it should "get a connection from the datasource and add it to the monad" in {
    val dataSource = mock[DataSource]
    val c = mock[Connection]
    when(dataSource.getConnection) thenReturn c

    connection[SimpleClosable] apply dataSource shouldBe SimpleClosable(c, Seq(c))
  }

  behavior of "jdbc.statement"

  it should "get a statement from the datasource and add it to the monad" in {
    val c = mock[Connection]
    val s = mock[Statement]
    when(c.createStatement) thenReturn s

    statement[SimpleClosable] apply c shouldBe SimpleClosable(s, Seq(s))
  }
  behavior of "jdbc.prepare"

  it should "get a prepared statement from the datasource and add it to the monad" in {
    val c = mock[Connection]
    val s = mock[PreparedStatement]
    val sql = "someSql"
    when(c.prepareStatement(sql)) thenReturn s

    prepare[SimpleClosable](sql) apply c shouldBe SimpleClosable(s, Seq(s))
  }

  behavior of "jdbc.execute"

  it should "execute the sql and " in {
    val s = mock[Statement]
    //    val rs = mock[ResultSet]
    val sql = "someSql"
    when(s.execute(sql)) thenReturn false

    execute(sql) apply s shouldBe false
  }

  behavior of "jdbc.toResultset"

  it should "execute the sql and " in {
    val s = mock[Statement]
    val rs = mock[ResultSet]
    val sql = "someSql"
    when(s.executeQuery(sql)) thenReturn rs

    toResultSet[SimpleClosable](sql) apply s shouldBe SimpleClosable(rs, Seq(rs))
  }

  it should "have a toSingleResultSet which calls next on the result set" in {
    val rs = mock[ResultSet]
    when(rs.next()) thenReturn true

    toSingleResultSet apply rs shouldBe rs
    verify(rs, times(1)).next()
  }

  it should "have a list method what passes the result set to a function as long as the next returns true" in {
    val rs = mock[ResultSet]
    when(rs.next()) thenReturn(true, true, false)
    when(rs.getString("colName")) thenReturn("one", "two")
    toList(rs => rs.getString("colName") + "_processed")(rs) shouldBe List("one_processed", "two_processed")
  }


}
