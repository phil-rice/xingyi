package one.xingyi.core.databaseService

import java.sql.{CallableStatement, Connection, ResultSet}

import javax.sql.DataSource
import one.xingyi.core.language.AnyLanguage._
import one.xingyi.core.monad.Liftable

import scala.language.higherKinds


case class SqlAndParams(sql: String, params: Seq[String], fakeResults: Option[QueryResults] = None)

object SqlAndParams {
  def insertStatement(table: String, columns: String*): SqlAndParams =
    SqlAndParams(s"insert into $table (${columns.mkString(",")}) values (${columns.map(_ => "?")})", columns)

}

//The Results in this signature are 'fake results'
trait StoredProcedureKleisliFactory[M[_], Req <: DatabaseRequest, Res] extends ((DataSource, Map[String, SqlAndParams], ((CallableStatement, Option[QueryResults]) => Res)) => Req => M[Res])


object StoredProcedureKleisliFactory {
  def setParams(statement: CallableStatement, params: Seq[(Int, String)]) = params.foreach { case (k, v) => statement.setString(k, v) }

  implicit def defaultStoredProcedureKleisliFactory[M[_] : Liftable, Req <: DatabaseRequest, Res]: StoredProcedureKleisliFactory[M, Req, Res] = {
    (dataSource, nameToSql, fn) =>
      req =>
        val SqlAndParams(sql, expectedParams, fakeResults) = nameToSql.getOrElse(req.name, throw new RuntimeException(s"Don't know how to process the sql with name ${req.name}"))
        val connection: Connection = dataSource.getConnection
        val statement = connection.prepareCall(sql)
        try {
          val actualParams = expectedParams.zipWithIndex.map { case (expected, i) => (i + 1, req.map.getOrElse(expected, throw new RuntimeException(s"Cannot find parameter $expected in $req"))) }
          setParams(statement, actualParams)
          fn(statement, fakeResults).liftM[M]
        } finally ({
          statement.close()
          connection.close()
        })
  }
  def update[M[_]](dataSource: DataSource, nameToSql: Map[String, SqlAndParams])
                  (implicit factory: StoredProcedureKleisliFactory[M, UpdateRequest, UpdateResponse]): SPKleisli[M, UpdateRequest, UpdateResponse] =
    factory(dataSource, nameToSql, (s, _) => UpdateResponse(s.executeUpdate()))

  def query[M[_]](dataSource: DataSource, nameToSql: Map[String, SqlAndParams])
                 (implicit factory: StoredProcedureKleisliFactory[M, QueryRequest, QueryResponse], resultSetToResults: ResultSetToResults): SPKleisli[M, QueryRequest, QueryResponse] =
    factory(dataSource, nameToSql, ((s, fake) => {
      val resultSet = s.executeQuery() //don't inline. We want this to be executed
      QueryResponse(fake.getOrElse(resultSetToResults(resultSet)))
    }))

  def storedProcedure[M[_]](dataSource: DataSource, nameToSql: Map[String, SqlAndParams])
                           (implicit factory: StoredProcedureKleisliFactory[M, StoredProcedureRequest, StoredProcedureResponse], resultSetToResults: ResultSetToResults): SPKleisli[M, StoredProcedureRequest, StoredProcedureResponse] =
    factory(dataSource, nameToSql, ((s, fake) => {
      val resultSet = s.executeQuery() //don't inline. We want this to be executed
      StoredProcedureResponse(fake.getOrElse(resultSetToResults(resultSet)))
    }))
}

case class QueryResults(title: List[String], values: List[List[String]])
object QueryResults {
  def oneRow(nameAndValues: (String, String)*): QueryResults = QueryResults(nameAndValues.toList.map(_._1), List(nameAndValues.toList.map(_._2)))
}
trait ResultSetToResults {
  def apply(result: ResultSet): QueryResults
}
object ResultSetToResults {
  implicit object resultSetToResults extends ResultSetToResults {
    override def apply(resultSet: ResultSet): QueryResults = {
      val metaData = resultSet.getMetaData
      val title = (1 to metaData.getColumnCount).toList.map(col => metaData.getColumnName(col))
      var values = List[List[String]]()
      while (resultSet.next())
        values = (1 to metaData.getColumnCount).toList.map(col => resultSet.getString(col)) :: values
      QueryResults(title, values.reverse)
    }
  }
}

trait ResultsToT[T] extends (QueryResults => T)
object ResultsToT {

  implicit val resultSetToMap: ResultsToT[Map[String, String]] = { results =>
    val values = results.values.headOption.getOrElse(throw new RuntimeException("no values in the result set" + results))
    results.title.zip(values).toMap
  }
  implicit val resultSetToResults: ResultsToT[QueryResults] = { results => results }
}

