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
    def jsonToStream: JsonToStream[SchemaForTest, T]
    val tx: ValueFromMultipleAliasFields[T]
  }
  case class SchemaItem[T](key: String)(implicit val jsonToStream: JsonToStream[SchemaForTest, T], val tx: ValueFromMultipleAliasFields[T]) extends SchemaForTest[T]
  case class SchemaItemWithChildren(key: String, hasMany: Boolean, children: List[SchemaForTest[_]])
                                   (implicit val jsonToStream: JsonToStream[SchemaForTest, Placeholder],
                                    val tx: ValueFromMultipleAliasFields[Placeholder]) extends SchemaForTest[Placeholder]

  object SchemaForTest {

    implicit def getPattern: GetPattern[SchemaForTest] = (s: SchemaForTest[_]) => Some(s.key)
    def parseToTableNameAndFiles[T](s: String): List[(Alias, List[FieldType[_]])] = {
      val mainSplitter = Strings.split(";")
      val fieldsSplitter = Strings.split(",")
      mainSplitter(s).flatMap { tf =>
        if (tf.contains("/")) {
          val (name, fields) = Strings.splitInTwo("/")(tf)
          List((Alias(name), fieldsSplitter(fields).map(FieldType.apply)))
        }
        else List()
      }
    }
    type JContext = String

    implicit def JsonToStreamFor: JsonToStreamFor[SchemaForTest] = new JsonToStreamFor[SchemaForTest] {
      override def putToJson[T](s: SchemaForTest[T]): JsonToStream[SchemaForTest, T] = s.jsonToStream
    }
    implicit val toTableAndFieldTypes: ToAliasAndFieldTypes[SchemaForTest] = new ToAliasAndFieldTypes[SchemaForTest] {
      override def apply[T](s: SchemaForTest[T]): List[AliasAndFieldTypes[T]] =
        parseToTableNameAndFiles(s.key).
          map { case (alias, fields) => AliasAndFieldTypes[T](alias, fields)(s.tx) }
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

    implicit object ObjectKeyMapForTest extends SchemaMapKey[SchemaForTest] {
      override def childKey[T](t: SchemaForTest[T]): String = t.key
      override def children[T](t: SchemaForTest[T]): List[SchemaForTest[_]] = t match {
        case SchemaItemWithChildren(_, _, children) => children
        case _ => Nil
      }
    }
  }

}