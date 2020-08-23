/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.jdbc

import java.sql.{Connection, ResultSet, Statement}

import javax.sql.DataSource
import one.xingyi.core.UtilsSpec
import one.xingyi.core.closable.{ClosableLanguage, SimpleClosable}
import org.mockito.Mockito.{times, verify, when}

import scala.util.Random

abstract class AbstractJdbcOpsSpec[Source](implicit jdbcOps: JdbcOps[Source]) extends UtilsSpec with ClosableFixture with Jdbc with ClosableLanguage {
  import jdbcOps._
  def getSourceFrom(d: DataSource): Source
  def expectedCloseConnectionCount: Int
  it should "have an executeSql that does the chain 'connection', 'statement', 'execute' and then returns the boolean from the execute" in {
    val d = mock[DataSource]
    val c = mock[Connection]
    val s = mock[Statement]
    val sql = "someSql"
    when(d.getConnection()) thenReturn c
    when(c.createStatement()) thenReturn s
    val n = Random.nextBoolean()
    when(s.execute(sql)) thenReturn n

    executeSql[SimpleClosable](sql) apply getSourceFrom(d) shouldBe n
    verify(s, times(1)) execute sql
    verify(s, times(1)).close()
    verify(c, times(expectedCloseConnectionCount)).close()
  }

  it should "have an getValue that does the chain 'connection', 'statement', 'execute' and then returns the boolean from the execute" in {
    val d = mock[DataSource]
    val c = mock[Connection]
    val s = mock[Statement]
    val rs = mock[ResultSet]
    val sql = "someSql"
    when(d.getConnection()) thenReturn c
    when(c.createStatement()) thenReturn s
    when(s.executeQuery(sql)) thenReturn rs
    when(rs.next()) thenReturn true
    when(rs.getString("colName")) thenReturn "value"

    getValue[SimpleClosable, String](sql, _.getString("colName")) apply getSourceFrom(d) shouldBe "value"
    verify(rs, times(1)).close()
    verify(s, times(1)).close()
    verify(c, times(expectedCloseConnectionCount)).close()
  }
  it should "have an getList that does the chain and then returns the result" in {
    val d = mock[DataSource]
    val c = mock[Connection]
    val s = mock[Statement]
    val rs = mock[ResultSet]
    val sql = "someSql"
    when(d.getConnection()) thenReturn c
    when(c.createStatement()) thenReturn s
    when(s.executeQuery(sql)) thenReturn rs
    when(rs.next()) thenReturn(true, true, false)
    when(rs.getString("colName")) thenReturn("one", "two")

    getList[SimpleClosable, String](sql)(_.getString("colName")) apply getSourceFrom(d) shouldBe List("one", "two")
    verify(rs, times(1)).close()
    verify(s, times(1)).close()
    verify(c, times(expectedCloseConnectionCount)).close()
  }

}

class ConnectionJdbcOps extends AbstractJdbcOpsSpec[Connection] {
  override def getSourceFrom(d: DataSource): Connection = d.getConnection
  override def expectedCloseConnectionCount: Int = 0
}
class DatasourceJdbcOps extends AbstractJdbcOpsSpec[DataSource] {
  override def getSourceFrom(d: DataSource): DataSource = d
  override def expectedCloseConnectionCount: Int = 1
}
