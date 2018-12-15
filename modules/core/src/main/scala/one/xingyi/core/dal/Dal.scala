/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.dal
import java.sql.{Date, PreparedStatement, ResultSet}

import javax.sql.DataSource

sealed trait Digest
case object DigestYes extends Digest
case object DigestNo extends Digest

trait AddToUpsert[Upsert, C] extends ((Upsert, Int, String, C) => Upsert)
object AddToUpsert {
  implicit def addToPsForString: AddToUpsert[PreparedStatement, String] = { (u, i, name, c) => u.setString(i, c); u }
  implicit def addToPsForInt: AddToUpsert[PreparedStatement, Int] = { (u, i, name, c) => u.setInt(i, c); u }
  implicit def addToPsForDouble: AddToUpsert[PreparedStatement, Double] = { (u, i, name, c) => u.setDouble(i, c); u }
  implicit def addToPsForDate: AddToUpsert[PreparedStatement, Date] = { (u, i, name, c) => u.setDate(i, c); u }
  implicit def addToPsForOption[T](implicit addTo: AddToUpsert[PreparedStatement, T]): AddToUpsert[PreparedStatement, Option[T]] = { (u, i, name, c) => c match {case None => u.setObject(i, null); case Some(t) => addTo(u, i, name, t)}; u }
}

trait GetFromResult[Result, C] extends ((Result, Int, String) => C)
object GetFromResult {
  implicit def getFromRsForString: GetFromResult[ResultSet, String] = (r, i, name) => r.getString(name)
  implicit def getFromRsForInt: GetFromResult[ResultSet, Int] = (r, i, name) => r.getInt(name)
  implicit def getFromRsForDouble: GetFromResult[ResultSet, Double] = (r, i, name) => r.getDouble(name)
  implicit def getFromRsForDate: GetFromResult[ResultSet, Date] = (r, i, name) => r.getDate(name)
  implicit def getFromRsForOption[T](implicit get: GetFromResult[ResultSet, T]): GetFromResult[ResultSet, Option[T]] = { (u, i, name) => Option(get(u, i, name)) }
}

trait SchemaCell[T] {
  def name: String
  def pos: Int
  def digest: Digest

}
trait Schema[Upsert, Result, T] {
  def tableName: String
  def read: Result => T
  def add(t: T): Upsert => Upsert
  def cells: Seq[SchemaCell[T]]
}
case class Dal[Upsert, Result]() {
  case class ForType[T](tableName: String) {
    type UpsertFn[C] = (Upsert, C) => Upsert
    trait DalCell[C] extends SchemaCell[T] {
      def fromT: T => C
      def add: AddToUpsert[Upsert, C]
      def get: GetFromResult[Result, C]
      def addFromT(upsert: Upsert, t: T) = add(upsert, pos, name, fromT(t))
      def getFrom(result: Result) = get(result, pos, name)
    }
    case class DalSchema(tableName: String, read: Result => T, cells: DalCell[_]*) extends Schema[Upsert, Result, T] {
      def add(t: T) = { upsert: Upsert => cells.foldLeft(upsert)((u, c) => c.addFromT(u, t)) }
    }
    case class cell[C1](name: String, fromT: T => C1, digest: Digest = DigestYes)(implicit val get: GetFromResult[Result, C1], val add: AddToUpsert[Upsert, C1]) extends DalCell[C1] {
      protected val c1 = this;
      val pos = 1
      def schema(readFn: C1 => T): Schema[Upsert, Result, T] = DalSchema(tableName, { r: Result => readFn(c1.getFrom(r)) }, c1)
      case class cell[C2](name: String, fromT: T => C2, digest: Digest = DigestYes)(implicit val get: GetFromResult[Result, C2], val add: AddToUpsert[Upsert, C2]) extends DalCell[C2] {
        protected val c2 = this;
        val pos = 2
        def schema(readFn: (C1, C2) => T): Schema[Upsert, Result, T] = DalSchema(tableName, { r: Result => readFn(c1.getFrom(r), c2.getFrom(r)) }, c1, c2)
        case class cell[C3](name: String, fromT: T => C3, digest: Digest = DigestYes)(implicit val get: GetFromResult[Result, C3], val add: AddToUpsert[Upsert, C3]) extends DalCell[C3] {
          protected val c3 = this;
          val pos = 3
          def schema(readFn: (C1, C2, C3) => T): Schema[Upsert, Result, T] = DalSchema(tableName, { r: Result => readFn(c1.getFrom(r), c2.getFrom(r), c3.getFrom(r)) }, c1, c2, c3)
          case class cell[C4](name: String, fromT: T => C4, digest: Digest = DigestYes)(implicit val get: GetFromResult[Result, C4], val add: AddToUpsert[Upsert, C4]) extends DalCell[C4] {
            protected val c4 = this;
            val pos = 4
            def schema(readFn: (C1, C2, C3, C4) => T): Schema[Upsert, Result, T] = DalSchema(tableName, { r: Result => readFn(c1.getFrom(r), c2.getFrom(r), c3.getFrom(r), c4.getFrom(r)) }, c1, c2, c3, c4)
            case class cell[C5](name: String, fromT: T => C5, digest: Digest = DigestYes)(implicit val get: GetFromResult[Result, C5], val add: AddToUpsert[Upsert, C5]) extends DalCell[C5] {
              protected val c5 = this;
              val pos = 5
              def schema(readFn: (C1, C2, C3, C4, C5) => T): Schema[Upsert, Result, T] = DalSchema(tableName, { r: Result => readFn(c1.getFrom(r), c2.getFrom(r), c3.getFrom(r), c4.getFrom(r), c5.getFrom(r)) }, c1, c2, c3, c4, c5)
            }
          }
        }
      }
    }
  }
}

//object JdbcSchemaOps {
//  def makeInsertStatement[T](schema: Schema[PreparedStatement, ResultSet, T]) = {
//    import schema._
//    s"insert into $tableName" + cells.map(_.name).mkString("(", ",", ")") + " values " + cells.map(_ => "?").mkString("(", ",", ")")
//  }
//  def makeReadStatement[T](schema: Schema[PreparedStatement, ResultSet, T]) = {
//    import schema._
//    s"select " + cells.map(_.name).mkString(",") + s"from  $tableName "
//  }
//
//  import one.xingyi.core.closable.ClosableLanguage._
//  import one.xingyi.core.closable.SimpleClosable._
//  import one.xingyi.core.jdbc.Jdbc._
//  def insert[T](e: T)(implicit s: Schema[PreparedStatement, ResultSet, T]): DataSource => Boolean = connection |===> prepare(makeInsertStatement(s)) |=> s.add(e) |=> executePS |===> result
//}

//case class ExampleThing(one: String, two: Int, three: Double, four: Option[String])
//object ExampleDal {
//  import JdbcSchemaOps._
//
//  implicit val s = Dal[PreparedStatement, ResultSet]().ForType[ExampleThing]("example").cell("one", _.one).cell("two", _.two).cell("three", _.three).cell("four", _.four).schema(ExampleThing.apply)
//  val e = ExampleThing("oneV", 2, 3.0, Some("string"))
//
//  insert(e)
//
//}
