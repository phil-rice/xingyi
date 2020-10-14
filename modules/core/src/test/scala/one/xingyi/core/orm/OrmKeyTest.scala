package one.xingyi.core.orm

import java.io.{ByteArrayOutputStream, OutputStream}

import one.xingyi.core.UtilsSpec
import one.xingyi.core.aggregate.{HasChildren, HasChildrenForHolder}
import one.xingyi.core.strings.Strings

import scala.language.{higherKinds, implicitConversions}


trait OrmKeyFixture extends UtilsSpec {
//  implicit def multipleFieldTx: OrmValueTransformer[String] = (ftis, data) => ftis.map(fti => fti.fieldType.name + ":" + data(fti.index)).mkString(",")

  sealed trait SchemaForTest[T] {
    def key: String
    def jsonToStream: JsonToStream[String, SchemaForTest, T]
    val tx: ValueFromMultipleTableFields[String, T]
  }
  case class SchemaItem[T](key: String)(implicit val jsonToStream: JsonToStream[String, SchemaForTest, T], val tx: ValueFromMultipleTableFields[String, T]) extends SchemaForTest[T]
  case class SchemaItemWithChildren(key: String, hasMany: Boolean, children: List[SchemaForTest[_]])
                                   (implicit val jsonToStream: JsonToStream[String, SchemaForTest, Placeholder], val tx: ValueFromMultipleTableFields[String, Placeholder]) extends SchemaForTest[Placeholder]

  object SchemaForTest {
//    def parse[T](s: String)(implicit ormValueTransformer: OrmValueTransformer[T]): List[OrmValueGetter[_]] =
//      parseToTableNameAndFiles[T](s).map { case (tn, fields) => OrmValueGetter(tn, fields)(ormValueTransformer) }

    def parseToTableNameAndFiles[T](s: String): List[(TableName, List[FieldType[_]])] = {
      val mainSplitter = Strings.split(";")
      val fieldsSplitter = Strings.split(",")
      mainSplitter(s).flatMap { tf =>
        if (tf.contains("/")) {
          val (name, fields) = Strings.splitInTwo("/")(tf)
          List((TableName(name, ""), fieldsSplitter(fields).map(FieldType.apply)))
        }
        else List()
      }
    }
//    implicit val findKeys: FindOrmEntityAndField[SchemaForTest] = new FindOrmEntityAndField[SchemaForTest] {
//      override def apply[T](s: SchemaForTest[T]): List[OrmValueGetter[_]] = SchemaForTest.parse(s.key)
//    }
    type JContext=String
    implicit def JsonToStreamFor: JsonToStreamFor[JContext, SchemaForTest] = new JsonToStreamFor[JContext, SchemaForTest] {
      override def putToJson[T](context: JContext, s: SchemaForTest[T]): JsonToStream[String, SchemaForTest, T] = s.jsonToStream
    }
    implicit val toTableAndFieldTypes: ToTableAndFieldTypes[JContext, SchemaForTest] = new ToTableAndFieldTypes[JContext, SchemaForTest] {
      override def apply[T](s: SchemaForTest[T]): List[TableAndFieldTypes[JContext, T]] =
        parseToTableNameAndFiles(s.key).
          map { case (tn, fields) => TableAndFieldTypes[String,T](tn, fields)(s.tx) }
    }


    implicit val isLink: IsLinkFieldFilter[SchemaForTest] = new IsLinkFieldFilter[SchemaForTest] {
      def apply[T](f: SchemaForTest[T]): Boolean = hasChildren(f).isEmpty && (f.key endsWith "c")
    }
    implicit def hasChildren: HasChildrenForHolder[SchemaForTest] = new HasChildrenForHolder[SchemaForTest] {
      override def apply[T](f: SchemaForTest[T]): List[SchemaForTest[_]] = f match {
        case s: SchemaItemWithChildren => s.children
        case _ => Nil
      }
    }

    implicit def debugArray: AsDebugString[SchemaForTest] = new AsDebugString[SchemaForTest] {
      override def apply[T](t: SchemaForTest[T]): String = "{" + t.key + "}"
      override def data(t: Any): String = t.toString
    }
    implicit object ObjectKeyMapForTest extends SchemaMapKey[SchemaForTest] {
      override def childKey[T](t: SchemaForTest[T]): String = t.key
      override def children[T](t: SchemaForTest[T]): ChildrenInSchema[SchemaForTest] = t match {
        case item: SchemaItem[_] => Zero()
        case SchemaItemWithChildren(_, true, children) => ZeroOrMore(children)
        case SchemaItemWithChildren(_, false, children) => AlwaysOne(children)
      }
    }
  }
  def checkKeys[Schema[_]](n: OrmKeys[Schema])(expected: String) =
    checkStrings(n.prettyPrint(""), expected)

  def checkArray[Schema[_] : AsDebugString](key: OrmKeys[Schema], a: Array[Any])(expected: String) = {
    require(key != null)
    require(a != null)
    checkStrings(key.printArray("", a).mkString("\n"), expected)
  }
}
