/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.databaseService

import java.sql.{CallableStatement, ResultSet}

import javax.sql.DataSource
import one.xingyi.core.monad.IdentityMonad
import org.mockito
import org.mockito.{ArgumentCaptor, Mockito}
import org.scalatest.{FlatSpec, Matchers}
import org.scalatestplus.mockito.MockitoSugar

class KlesliTest extends FlatSpec with Matchers with MockitoSugar {

  val nameToSql: Map[String, SqlAndParams] = Map()


  behavior of "Kleisli.update"


  it should "return the result of passing the datasource and the nameToSql to the stored procedure factory, and the result of that should executeUpdate" in {
    implicit val factory = mock[StoredProcedureKleisliFactory[IdentityMonad, UpdateRequest, UpdateResponse]]
    val dataSource = mock[DataSource]
    val capture = new ArgumentCaptor[(CallableStatement, Option[QueryResults]) => UpdateResponse]()
    val kleisli = mock[UpdateRequest => IdentityMonad[UpdateResponse]]

    Mockito.when(factory.apply(mockito.Matchers.eq(dataSource), mockito.Matchers.eq(nameToSql), capture.capture())).thenReturn(kleisli)
    StoredProcedureKleisliFactory.update(dataSource, nameToSql) shouldBe kleisli

    val statement = mock[CallableStatement]
    Mockito.when(statement.executeUpdate()).thenReturn(123)

    capture.getValue.apply(statement, None) shouldBe UpdateResponse(123)
    capture.getValue.apply(statement, Some(mock[QueryResults])) shouldBe UpdateResponse(123)
  }


  behavior of "Kleisi.query"

  it should "return the result of passing the datasource and the nameToSql to the stored procedure factory, and the result of that should executeQuery" in {
    implicit val factory = mock[StoredProcedureKleisliFactory[IdentityMonad, QueryRequest, QueryResponse]]
    implicit val resultSetToResults = mock[ResultSetToResults]
    val dataSource = mock[DataSource]
    val capture = new ArgumentCaptor[(CallableStatement, Option[QueryResults]) => QueryResponse]()
    val kleisli = mock[QueryRequest => IdentityMonad[QueryResponse]]

    Mockito.when(factory.apply(mockito.Matchers.eq(dataSource), mockito.Matchers.eq(nameToSql), capture.capture())).thenReturn(kleisli)
    implicit val resultSetToString: ResultsToT[String] = mock[ResultsToT[String]]

    StoredProcedureKleisliFactory.query(dataSource, nameToSql) shouldBe kleisli

    val statement = mock[CallableStatement]

    val resultSet = mock[ResultSet]
    val results = mock[QueryResults]
    Mockito.when(resultSetToResults.apply(resultSet)).thenReturn(results)
    Mockito.when(statement.executeQuery()).thenReturn(resultSet)
    Mockito.when(resultSetToString.apply(results)).thenReturn("result")
    capture.getValue.apply(statement, None) shouldBe QueryResponse(results)

    Mockito.verify(resultSet, Mockito.times(0)).next()
  }

}
