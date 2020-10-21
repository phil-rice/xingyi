package one.xingyi.core.orm

import java.sql.PreparedStatement

import one.xingyi.core.UtilsSpec
import org.mockito.Mockito

class IDWhereForTableTest extends UtilsSpec {

  behavior of classOf[IDWhereForTable].getSimpleName

  val whereA1 = IDWhereForTable(Keys("a"), List(1))
  val whereAb34 = IDWhereForTable(Keys("a,b"), List(3, 4))
  it should "make where sql" in {
    whereA1.whereSql("A") shouldBe "A.a=?"
    whereAb34.whereSql("A") shouldBe "A.a=? and A.b=?"
  }

  it should "set prepared statements" in {
    val statement = mock[PreparedStatement]
    whereAb34.setParams(statement)
    Mockito.verify(statement, Mockito.times(1)).setObject(1, 3)
    Mockito.verify(statement, Mockito.times(1)).setObject(2, 4)
  }

}
