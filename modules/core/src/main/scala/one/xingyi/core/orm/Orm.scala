/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.orm

import java.sql.{Connection, ResultSet}

import javax.sql.DataSource
import one.xingyi.core.closable.ClosableM
import one.xingyi.core.jdbc.JdbcOps

import scala.annotation.tailrec
import scala.language.higherKinds

trait StreamEntity[T] extends (MainEntity => Stream[T])
object StreamEntity {
  def apply[T](batchConfig: OrmBatchConfig)(implicit ormMaker: OrmMaker[T], fastReaderOps: FastReaderDal, sqlOps: FastOrmSql) =
    new SimpleStreamEntity[T](FastReader(batchConfig))
}

class SimpleStreamEntity[T](fastReader: FastReader[T])(implicit ormMaker: OrmMaker[T], fastReaderOps: FastReaderDal, sqlOps: FastOrmSql) extends StreamEntity[T] {
  override def apply(mainEntity: MainEntity): Stream[T] = stream(mainEntity, 0)

  private def stream(mainEntity: MainEntity, n: Int): Stream[T] = {
    val subStream: Stream[T] = fastReader(mainEntity)(n)
    if (subStream.isEmpty) subStream else subStream #::: stream(mainEntity, n + 1)
  }

}

trait FastReader[T] extends (MainEntity => Int => Stream[T])
object FastReader {
  def apply[T](batchConfig: OrmBatchConfig)(implicit ormMaker: OrmMaker[T], fastReaderOps: FastReaderDal, sqlOps: FastOrmSql) =
    new FastReaderImpl[T](batchConfig)
}

trait FastReaderDal extends {
  def execute(ds: Connection): String => Unit
  def query(ds: Connection): String => List[List[AnyRef]]
}
object FastReaderDal {
  implicit def defaultFastReaderOps[M[_] : ClosableM](implicit jdbcOps: JdbcOps[Connection]) = new FastReaderDal {
    import jdbcOps._

    def execute(c: Connection): String => Unit = { s: String => executeSql(s) apply c }
    def query(c: Connection): String => List[List[AnyRef]] = { s: String => getList(s) { rs: ResultSet => (1 to rs.getMetaData.getColumnCount).toList.map(rs.getObject) } apply c }
  }
}

case class OrmBatchConfig(dataSource: DataSource, batchSize: Int) {
  require(dataSource != null)
}

case class BatchDetails(batchSize: Int, index: Int) {
  def offset: Int = batchSize * index
}
class FastReaderImpl[T](batchConfig: OrmBatchConfig)(implicit ormMaker: OrmMaker[T], fastReaderOps: FastReaderDal, sqlOps: FastOrmSql) extends FastReader[T] {
  import fastReaderOps._

  override def apply(main: MainEntity): Int => Stream[T] = { n: Int =>
    val connection = batchConfig.dataSource.getConnection
    try {
      OrmStrategies.dropTempTables.map(execute(connection)).walk(main)
      OrmStrategies.createTempTables(BatchDetails(batchConfig.batchSize, n)).map(execute(connection)).walk(main)
      ormMaker(OrmStrategies.drainTempTables.map(query(connection)).walk(main).toMap)
    } finally {
      connection.close()
    }
  }
}



