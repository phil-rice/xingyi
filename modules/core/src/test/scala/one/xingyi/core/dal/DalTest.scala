package one.xingyi.core.dal
import java.sql.{Date, PreparedStatement, ResultSet}
import java.util

import one.xingyi.core.UtilsSpec
import org.mockito.Mockito._

class DalTest extends UtilsSpec {
  val date = Date.valueOf("2017-1-1")

  val s1 = Dal[PreparedStatement, ResultSet]().ForType[Tuple1[String]]("someTableName").
    cell[String]("cell1", _._1).schema(s => Tuple1(s))
  val s2 = Dal[PreparedStatement, ResultSet]().ForType[Tuple2[String, Int]]("someTableName").
    cell[String]("cell1", _._1).cell[Int]("cell2", _._2).schema((c1, c2) => (c1, c2))
  val s3 = Dal[PreparedStatement, ResultSet]().ForType[Tuple3[String, Int, Int]]("someTableName").
    cell[String]("cell1", _._1).cell[Int]("cell2", _._2).cell[Int]("cell3", _._3).schema((c1, c2, c3) => (c1, c2, c3))
  val s4 = Dal[PreparedStatement, ResultSet]().ForType[Tuple4[String, Int, Int, Int]]("someTableName").
    cell[String]("cell1", _._1).cell[Int]("cell2", _._2).cell[Int]("cell3", _._3).cell[Int]("cell4", _._4).schema((c1, c2, c3, c4) => (c1, c2, c3, c4))
  val s5 = Dal[PreparedStatement, ResultSet]().ForType[Tuple5[String, Int, Double, Date, Option[String]]]("someTableName").
    cell[String]("cell1", _._1).cell[Int]("cell2", _._2).cell[Double]("cell3", _._3).cell[Date]("cell4", _._4).cell[Option[String]]("cell5", _._5).schema((c1, c2, c3, c4, c5) => (c1, c2, c3, c4, c5))

  behavior of "Dal one cell"

  it should "create a schema " in {
    s1.tableName shouldBe "someTableName"
    val Seq(c1) = s1.cells
    c1.name shouldBe "cell1"
    c1.digest shouldBe DigestYes
    c1.pos shouldBe 1
  }
  it should "add cells to a prepared statement" in {
    val ps = mock[PreparedStatement]
    s1.add(Tuple1("someString"))(ps)
    verify(ps, times(1)).setString(1, "someString")
  }
  it should "read from a result Set" in {
    val rs = mock[ResultSet]
    when(rs.getString("cell1")) thenReturn "someValue"
    s1.read(rs) shouldBe Tuple1("someValue")
  }
  behavior of "Dal two cell"

  it should "create a schema " in {
    s2.tableName shouldBe "someTableName"
    val Seq(c1, c2) = s2.cells
    c1.name shouldBe "cell1"
    c2.name shouldBe "cell2"
    c1.digest shouldBe DigestYes
    c2.digest shouldBe DigestYes
    c1.pos shouldBe 1
    c2.pos shouldBe 2
  }
  it should "add cells to a prepared statement" in {
    val ps = mock[PreparedStatement]
    s2.add(("someString", 123))(ps)
    verify(ps, times(1)).setString(1, "someString")
    verify(ps, times(1)).setInt(2, 123)
  }
  it should "read from a result Set" in {
    val rs = mock[ResultSet]
    when(rs.getString("cell1")) thenReturn "someValue"
    when(rs.getInt("cell2")) thenReturn 123
    s2.read(rs) shouldBe("someValue", 123)
  }

  behavior of "Dal three cell"

  it should "create a schema " in {
    s3.tableName shouldBe "someTableName"
    val Seq(c1, c2, c3) = s3.cells
    c1.name shouldBe "cell1"
    c2.name shouldBe "cell2"
    c3.name shouldBe "cell3"
    c1.digest shouldBe DigestYes
    c2.digest shouldBe DigestYes
    c3.digest shouldBe DigestYes
    c1.pos shouldBe 1
    c2.pos shouldBe 2
    c3.pos shouldBe 3
  }
  it should "add cells to a prepared statement" in {
    val ps = mock[PreparedStatement]
    s3.add(("someString", 123, 456))(ps)
    verify(ps, times(1)).setString(1, "someString")
    verify(ps, times(1)).setInt(2, 123)
    verify(ps, times(1)).setInt(3, 456)
  }
  it should "read from a result Set" in {
    val rs = mock[ResultSet]
    when(rs.getString("cell1")) thenReturn "someValue"
    when(rs.getInt("cell2")) thenReturn 123
    when(rs.getInt("cell3")) thenReturn 456
    s3.read(rs) shouldBe("someValue", 123, 456)
  }

  behavior of "Dal four cell"

  it should "create a schema " in {
    s4.tableName shouldBe "someTableName"
    val Seq(c1, c2, c3, c4) = s4.cells
    c1.name shouldBe "cell1"
    c2.name shouldBe "cell2"
    c3.name shouldBe "cell3"
    c4.name shouldBe "cell4"
    c1.digest shouldBe DigestYes
    c2.digest shouldBe DigestYes
    c3.digest shouldBe DigestYes
    c4.digest shouldBe DigestYes
    c1.pos shouldBe 1
    c2.pos shouldBe 2
    c3.pos shouldBe 3
    c4.pos shouldBe 4
  }
  it should "add cells to a prepared statement" in {
    val ps = mock[PreparedStatement]
    s4.add(("someString", 123, 456, 7))(ps)
    verify(ps, times(1)).setString(1, "someString")
    verify(ps, times(1)).setInt(2, 123)
    verify(ps, times(1)).setInt(3, 456)
    verify(ps, times(1)).setInt(4, 7)
  }
  it should "read from a result Set" in {
    val rs = mock[ResultSet]
    when(rs.getString("cell1")) thenReturn "someValue"
    when(rs.getInt("cell2")) thenReturn 123
    when(rs.getInt("cell3")) thenReturn 456
    when(rs.getInt("cell4")) thenReturn 7
    s4.read(rs) shouldBe("someValue", 123, 456, 7)
  }

  behavior of "Dal five cell"

  it should "create a schema " in {
    s4.tableName shouldBe "someTableName"
    val Seq(c1, c2, c3, c4, c5) = s5.cells
    c1.name shouldBe "cell1"
    c2.name shouldBe "cell2"
    c3.name shouldBe "cell3"
    c4.name shouldBe "cell4"
    c5.name shouldBe "cell5"
    c1.digest shouldBe DigestYes
    c2.digest shouldBe DigestYes
    c3.digest shouldBe DigestYes
    c4.digest shouldBe DigestYes
    c5.digest shouldBe DigestYes
    c1.pos shouldBe 1
    c2.pos shouldBe 2
    c3.pos shouldBe 3
    c4.pos shouldBe 4
    c5.pos shouldBe 5
  }
  it should "add cells to a prepared statement" in {
    val ps = mock[PreparedStatement]
    s5.add(("someString", 123, 456, date, Some("another")))(ps)
    verify(ps, times(1)).setString(1, "someString")
    verify(ps, times(1)).setInt(2, 123)
    verify(ps, times(1)).setDouble(3, 456)
    verify(ps, times(1)).setDate(4, date)
    verify(ps, times(1)).setString(5, "another")
  }
  it should "read from a result Set" in {
    val rs = mock[ResultSet]
    when(rs.getString("cell1")) thenReturn "someValue"
    when(rs.getInt("cell2")) thenReturn 123
    when(rs.getDouble("cell3")) thenReturn 456
    when(rs.getDate("cell4")) thenReturn date
    when(rs.getString("cell5")) thenReturn "another"
    s5.read(rs) shouldBe("someValue", 123, 456, date, Some("another"))
  }
  behavior of "Dal five cell with 'none' as the optional string"

  it should "add cells to a prepared statement" in {
    val ps = mock[PreparedStatement]
    s5.add(("someString", 123, 456, date, None))(ps)
    verify(ps, times(1)).setString(1, "someString")
    verify(ps, times(1)).setInt(2, 123)
    verify(ps, times(1)).setDouble(3, 456)
    verify(ps, times(1)).setDate(4, date)
    verify(ps, times(1)).setObject(5, null)
  }
  it should "read from a result Set" in {
    val rs = mock[ResultSet]
    when(rs.getString("cell1")) thenReturn "someValue"
    when(rs.getInt("cell2")) thenReturn 123
    when(rs.getDouble("cell3")) thenReturn 456
    when(rs.getDate("cell4")) thenReturn date
    when(rs.getString("cell5")) thenReturn null
    s5.read(rs) shouldBe("someValue", 123, 456, date, None)
  }
}
