package one.xingyi.core.orm

import java.sql.{Connection, PreparedStatement, ResultSet}

import javax.sql.DataSource
import one.xingyi.core.closable.ClosableM
import one.xingyi.core.jdbc.{JdbcOps, SetParams}

import scala.language.higherKinds

trait FastReader[T] extends (MainEntity => Int => Stream[T])

object FastReader {
  def getOneBlockOfDataFromDs(dataSource: DataSource, main: MainEntity, size: Int)(index: Int)(implicit fastReaderOps: FastReaderDal, sqlOps: FastOrmSql): Map[OrmEntity, List[List[AnyRef]]] = {
    val batchConfig = OrmBatchConfig(dataSource, size)
    val connection = dataSource.getConnection
    try {
      FastReader.getOneBlockOfData(connection, main, batchConfig, index)
    } finally {
      connection.close()
    }
  }

  def getOneBlockOfData(connection: Connection, main: MainEntity, batchConfig: OrmBatchConfig, n: Int)(implicit fastReaderOps: FastReaderDal, sqlOps: FastOrmSql): Map[OrmEntity, List[List[AnyRef]]] = {
    import fastReaderOps._
    OrmStrategies.dropTempTables.map(execute(connection)).walk(main)
    val createSql = OrmStrategies.createTempTables(BatchDetails(batchConfig.batchSize, n, batchConfig.whereForTable)).walk(main).map(_._2)
    executePs(connection, batchConfig.whereForTable)(createSql.head)
    createSql.tail.foreach(execute(connection))
    OrmStrategies.drainTempTables.map(query(connection)).walk(main).toMap
  }

  def apply[T](batchConfig: OrmBatchConfig)(implicit ormMaker: OrmMaker[T], fastReaderOps: FastReaderDal, sqlOps: FastOrmSql) =
    new FastReaderImpl[T](batchConfig)
}

trait FastReaderDal extends {
  def execute(ds: Connection): String => Unit
  def executePs(ds: Connection, setParams: SetParams[PreparedStatement]): String => Unit
  def query(ds: Connection): String => List[List[AnyRef]]
}
object FastReaderDal {
  implicit def defaultFastReaderOps[M[_] : ClosableM](implicit jdbcOps: JdbcOps[Connection]): FastReaderDal = new FastReaderDal {
    import jdbcOps._

    def execute(c: Connection): String => Unit = { s: String => executeSql(s) apply c }
    def query(c: Connection): String => List[List[AnyRef]] = { s: String => getList(s) { rs: ResultSet => (1 to rs.getMetaData.getColumnCount).toList.map(rs.getObject) } apply c }
    override def executePs(c: Connection, setParams: SetParams[PreparedStatement]): String => Unit = { s: String => executeSqlForPs(s, setParams) apply c }
  }
}
