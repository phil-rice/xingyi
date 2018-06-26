package one.xingyi.core.jdbc

import javax.sql.DataSource
import one.xingyi.core.UtilsSpec
import org.scalatest.BeforeAndAfterAll

trait DatabaseSourceFixture[DS <: DataSource] extends UtilsSpec with BeforeAndAfterAll {
  def makeDataSource(): DS
  def closeDataSource(ds: DS)
  val ds: DS = makeDataSource()

  override protected def afterAll(): Unit = {
    closeDataSource(ds)
    super.afterAll()
  }

}