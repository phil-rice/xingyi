package one.xingyi.core.orm

import java.io.ByteArrayOutputStream

import one.xingyi.core.UtilsSpec
import one.xingyi.core.strings.Strings
import org.scalatest.Matchers

import scala.language.implicitConversions


class NumericKeyWithMultipleFieldsTest extends NumericKeySpecFixture {

  implicit def multipleFieldTx[T]: OrmValueTransformer[T] = (ftis, data) => ftis.map(fti => fti.fieldType.name + ":" + data(fti.index)).mkString(",")

  val threeValueField = SchemaItem("t1:v1,v2,v3")
  behavior of "SchemaItemParser - checking test DSL"
  val t1 = TableName("t1", "")
  val t2 = TableName("t2", "")
  it should "turn a string into tables/field list" in {
    type NotImportant=Any
    SchemaForTest.parse("") shouldBe List()
    SchemaForTest.parse("justAstring") shouldBe List()
    SchemaForTest.parse("one;two;three") shouldBe List()
    SchemaForTest.parse("t1/f1:int") shouldBe List(OrmValueGetter[NotImportant](t1, List(FieldType("f1:int"))))
    SchemaForTest.parse("t1/f1:int,f2") shouldBe List(OrmValueGetter[NotImportant](t1, List(FieldType("f1:int"), FieldType("f2"))))
    SchemaForTest.parse("t1/f1:int,f2,f3") shouldBe List(OrmValueGetter[NotImportant](t1, List(FieldType("f1:int"), FieldType("f2"), FieldType("f3"))))
    SchemaForTest.parse("t1/f1:int,f2;t2/f3,f4:int") shouldBe List(
      OrmValueGetter[NotImportant](t1, List(FieldType("f1:int"), FieldType("f2"))),
      OrmValueGetter[NotImportant](t2, List(FieldType("f3"), FieldType("f4:int"))))
  }

}
