/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.orm

import java.sql.ResultSet

import javax.sql.DataSource
import one.xingyi.core.closable.ClosableM
import one.xingyi.core.functions.Streams
import one.xingyi.core.jdbc.{Jdbc, JdbcOps}
import one.xingyi.core.jdbc.Jdbc._

import scala.language.higherKinds
trait FastReader[T] extends (MainEntity => Stream[T])
object FastReader {
  def apply[T](batchConfig: OrmBatchConfig)(implicit ormMaker: OrmMaker[T], fastReaderOps: FastReaderDal, sqlOps: FastOrmSql) = new FastReaderImpl[T](batchConfig)
}

// We might need to reconsider this interface. At the moment there is a different connection for each operation, but there is something cool about the idea of
// running the operations in a connection and the temp tables vanishing at the end of the connection
// And actually there is probably a bug waiting to happen here: the temp tables die when the connection actually closes.
//having checked.. this is just wrong. We need to do this all in the same connection.
trait FastReaderDal extends {
  def execute(ds: DataSource): String => Unit
  def query(ds: DataSource): String => List[List[AnyRef]]
}
object FastReaderDal {
  implicit def defaultFastReaderOps[M[_] : ClosableM](implicit jdbcOps: JdbcOps[DataSource]) = new FastReaderDal {
    import jdbcOps._
    def execute(ds: DataSource): String => Unit = { s: String => executeSql(s) apply ds }
    def query(ds: DataSource): String => List[List[AnyRef]] = { s: String => getList(s) { rs: ResultSet => (1 to rs.getMetaData.getColumnCount).toList.map(rs.getObject) } apply ds }
  }
}

case class OrmBatchConfig(dataSource: DataSource, batchSize: Int)

case class BatchDetails(batchSize: Int, index: Int) {
  def offset: Int = batchSize * index
}
class FastReaderImpl[T](batchConfig: OrmBatchConfig)(implicit ormMaker: OrmMaker[T], fastReaderOps: FastReaderDal, sqlOps: FastOrmSql) extends FastReader[T] {
  import batchConfig.dataSource
  import fastReaderOps._

  //TODO Clean this. There is some composition going on here that is interesting.
  def makeBatch(n: Int, main: MainEntity): Option[(List[T], MainEntity)] = {
    OrmStrategies.dropTempTables.map(execute(dataSource)).walk(main)
    OrmStrategies.createTempTables(BatchDetails(batchConfig.batchSize, n)).map(execute(dataSource)).walk(main)
    val list = ormMaker(OrmStrategies.drainTempTables.map(query(dataSource)).walk(main).toMap)
    if (list.isEmpty) None else Some((list, main))
  }
  override def apply(mainEntity: MainEntity): Stream[T] = Streams.unfoldIndexedList(0, mainEntity)(makeBatch)
}



