package one.xingyi.core.databaseService

import one.xingyi.core.FunctionFixture
import org.scalatest.{FlatSpec, Matchers}
import org.scalatestplus.mockito.MockitoSugar

class QueryStatementTest extends FlatSpec with Matchers with FunctionFixture with MockitoSugar {

//  behavior of "defaultQueryStatement"
//
//  it should "execute query, move the resultset to the first time and call the passed function to return T" in {
//    val resultSet = mock[ResultSet]
//    val statement = mock[CallableStatement]
//    Mockito.when(statement.executeQuery()).thenReturn(resultSet)
//    Mockito.when(resultSet.next()).thenReturn(true, false)
//
//    QueryStatement.defaultQueryStatement[String](fn1(resultSet, "result"))(statement) shouldBe "result"
//
//    Mockito.verify(resultSet, Mockito.times(1)).next()
//    Mockito.verify(resultSet, Mockito.times(1)).close()
//    Mockito.verify(statement, Mockito.times(0)).close()
//  }

}
