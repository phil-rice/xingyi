package one.xingyi.core.jdbc

import java.sql.{Connection, PreparedStatement, ResultSet, Statement}
import java.util.concurrent.atomic.AtomicInteger

import javax.sql.DataSource
import one.xingyi.core.closable.ClosableM

import scala.language.{higherKinds, postfixOps}
import one.xingyi.core.language.AnyLanguage._
import one.xingyi.core.closable.ClosableLanguage._

trait Jdbc {
  def connection[M[_] : ClosableM] = { d: DataSource => d.getConnection.liftClosable }
  def statement[M[_] : ClosableM] = { c: Connection => c.createStatement().liftClosable }
  def prepare[M[_] : ClosableM](sql: String) = { c: Connection => c.prepareStatement(sql).liftClosable }
  def execute(sql: String) = { s: Statement => s.execute(sql) }
  def toResultSet[M[_] : ClosableM](sql: String) = { s: Statement => s.executeQuery(sql).liftClosable }
  def toSingleResultSet = { resultSet: ResultSet =>
    if (!resultSet.next) throw new IllegalStateException()
    resultSet
  }
  def toList[X](fn: ResultSet => X): ResultSet => List[X] = { resultSet: ResultSet =>
    var list = List[X]()
    while (resultSet.next()) {
      list = fn(resultSet) :: list
    }
    list.reverse
  }

  def executeSql[M[_] : ClosableM](sql: String): DataSource => Boolean =
    connection |==> statement |=> execute(sql) |===> result
  def getValue[M[_] : ClosableM, X](sql: String)(fn: ResultSet => X): DataSource => X =
    connection |==> statement |==> toResultSet(sql) |=> toSingleResultSet |=> fn |===> result
  def getList[M[_] : ClosableM, X](sql: String)(fn: ResultSet => X): DataSource => List[X] =
    connection |==> statement |==> toResultSet(sql) |=> toList(fn) |===> result

  def process[M[_] : ClosableM, From, To](batchSize: Int)(readSql: String, readFn: ResultSet => From)(writeSql: String, preparer: To => List[Object])(fn: From => To): DataSource => M[Unit] =
    connection |==> inParallel(statement |==> toResultSet(readSql)).and(prepare(writeSql) |==> Batcher.jdbcInsert[M, To](batchSize, preparer)).merge(Batcher(readFn andThen fn))


}
case class BatchConfig[T](batchSize: Int, prepare: T => Unit, flush: () => Unit)
class Batcher[T](batchConfig: BatchConfig[T], count: AtomicInteger = new AtomicInteger(0)) extends (T => Unit) with AutoCloseable {
  import batchConfig._
  override def apply(t: T): Unit = prepare(t) sideeffectTry (_ => count.tick(batchSize)(flush())) get
  def close = count ifNotZero flush()
}


object Batcher {
  def apply[T](readFn: ResultSet => T)(resultSet: ResultSet)(batcher: Batcher[T]): Unit = while (resultSet.next()) batcher(readFn(resultSet))
  def jdbcInsert[M[_] : ClosableM, T](batchSize: Int, preparer: T => List[Object])(statement: PreparedStatement): M[Batcher[T]] =
    new Batcher[T](BatchConfig(batchSize,
      { t => preparer(t).zipWithIndex.foreach { case (o, i) => statement.setObject(i + 1, o) }; statement.addBatch() },
      { () => statement.executeBatch(); statement.clearBatch() })).liftClosable

}
