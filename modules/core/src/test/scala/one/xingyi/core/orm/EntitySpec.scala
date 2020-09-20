/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.orm

import one.xingyi.core.UtilsSpec

import scala.language.implicitConversions

trait EntityFixture {
  import FieldType._

  val personIdField = int("id")
  val nameField = string("name")

  val addressIdField = int("id")
  val addressPersonIdField = int("personId")
  val line1Field = string("line1")
  val line2Field = string("line2")


  val phoneIdField = int("id")
  val phonePersonIdField = int("personId")
  val manufacturerField = string("manufacturer")

  val phoneDetailsIdField = int("id")
  val purposeField = string("purpose")
  val telField = string("tel")

  implicit def fieldToKeys[T](f: FieldType[T]) = Keys(List(f))
  val addressEntity = OneToManyEntity("address", "a", addressIdField, addressPersonIdField, List(line1Field, line2Field), List())
  val phoneDetailsEntity = OneToManyEntity("phonedetails", "pd", phoneDetailsIdField, phonePersonIdField, List(purposeField, telField), List())
  val phoneEntity = OneToManyEntity("phone", "ph", phoneIdField, phonePersonIdField, List(manufacturerField), List(phoneDetailsEntity))
  val mainEntity = MainEntity("person", "p", personIdField, List(nameField), List(addressEntity, phoneEntity))
}

abstract class AbstractEntityTest[E <: OrmEntity] extends UtilsSpec {

  def entity(tableName: String, alias: String, primaryKey: Keys, otherKeys: Keys, dataFields: List[FieldType[_]], children: List[_ <: ChildEntity]): E
  //  def otherIds(keyToCopySize: Keys, prefix: String) = Keys(keyToCopySize.list.zipWithIndex.map { case (_, i) => FieldType(s"$prefix$i") })

  behavior of getClass.getSimpleName

  it should "have the primary key after the data fields in all fields: single key" in {
    val e = entity("sometable", "someAlias", Keys("pk"), Keys("other"), List(FieldType("a"), FieldType("b"), FieldType("c")), List())
    val fieldNames = e.fieldsForCreate.map(_.name)
    fieldNames.take(3) shouldBe List("a", "b", "c")
    fieldNames should contain("pk")
    fieldNames.toSet.size shouldBe fieldNames.size
  }
  it should "have the primary key after the data fields in all fields: multiple keys key" in {
    val e = entity("sometable", "someAlias", Keys("pk1,pk2"), Keys("other"), List(FieldType("a"), FieldType("b"), FieldType("c")), List())
    val fieldNames = e.fieldsForCreate.map(_.name)
    fieldNames.take(3) shouldBe List("a", "b", "c")
    fieldNames should contain("pk1")
    fieldNames should contain("pk2")
    fieldNames.toSet.size shouldBe fieldNames.size
  }

  it should "only have keys once even if the data fields have the same name" in {
    val e = entity("sometable", "someAlias", Keys("a,b,pk"), Keys("c,other"), List(FieldType("a"), FieldType("b"), FieldType("c")), List())
    val fieldNames = e.fieldsForCreate.map(_.name)
    withClue(fieldNames) {
      fieldNames.take(3) shouldBe List("a", "b", "c")
      fieldNames should contain("pk")
      fieldNames.toSet.size shouldBe fieldNames.size
    }
  }

  it should "have extra fields if they are added by (say) the many to one table" in {
    val m21 = ManyToOneEntity("otherTable", "otherAlias", Keys("whoCares"), Keys("added1,added2"), List(FieldType("x"), FieldType("y")), List())
    val e = entity("sometable", "someAlias", Keys("a,b,pk"), Keys("other"), List(FieldType("a"), FieldType("b"), FieldType("c")), List(m21))

    val fieldNames = e.fieldsForCreate.map(_.name)
    withClue(fieldNames) {
      fieldNames.take(3) shouldBe List("a", "b", "c")
      fieldNames should contain("pk")
      fieldNames should contain("added1")
      fieldNames should contain("added2")
      fieldNames shouldNot contain("x")
      fieldNames shouldNot contain("y")
      fieldNames shouldNot contain("whoCares")
      fieldNames.toSet.size shouldBe fieldNames.size
    }
  }
  it should "have a primaryKeyFieldsAndIndex" in {
    val e = entity("sometable", "someAlias", Keys("a,b,pk"), Keys("other"), List(FieldType("a"), FieldType("b"), FieldType("c")), List())
    val fieldNames = e.fieldsForCreate.map(_.name)
    e.primaryKeyFieldsAndIndex shouldBe KeysAndIndex(List(
      (0, FieldType("a")),
      (1, FieldType("b")),
      (fieldNames.indexOf("pk"), FieldType("pk"))))
  }
}

class MainEntityTest extends AbstractEntityTest[MainEntity] {
  override def entity(tableName: String, alias: String, primaryKey: Keys, otherKeys: Keys, dataFields: List[FieldType[_]], children: List[_ <: ChildEntity]): MainEntity =
    MainEntity(tableName, alias, primaryKey, dataFields, children)
}
class OneToManyTest extends AbstractEntityTest[OneToManyEntity] {
  override def entity(tableName: String, alias: String, primaryKey: Keys, otherKeys: Keys, dataFields: List[FieldType[_]], children: List[_ <: ChildEntity]) = {
    OneToManyEntity(tableName, alias, primaryKey, otherKeys, dataFields, children)
  }

  it should "have a parentIdsAndIndex" in {
    val e: OneToManyEntity = entity("sometable", "someAlias", Keys("a,b,pk"), Keys("p1,p2,c"), List(FieldType("a"), FieldType("b"), FieldType("c")), List())
    val fieldNames = e.fieldsForCreate.map(_.name)
    e.parentIdsAndIndex shouldBe KeysAndIndex(List(
      (fieldNames.indexOf("p1"), FieldType("p1")),
      (fieldNames.indexOf("p2"), FieldType("p2")),
      (fieldNames.indexOf("c"), FieldType("c"))))
  }

}
class ManyToOneTest extends AbstractEntityTest[ManyToOneEntity] {
  override def entity(tableName: String, alias: String, primaryKey: Keys, otherKeys: Keys, dataFields: List[FieldType[_]], children: List[_ <: ChildEntity]) = {
    ManyToOneEntity(tableName, alias, primaryKey, otherKeys, dataFields, children)
  }
}
class SameIdEntityTest extends AbstractEntityTest[SameIdEntity] {
  override def entity(tableName: String, alias: String, primaryKey: Keys, otherKeys: Keys, dataFields: List[FieldType[_]], children: List[_ <: ChildEntity]) = {
    SameIdEntity(tableName, alias, primaryKey, dataFields, children)
  }
}
class OneToZeroOneEntityTest extends AbstractEntityTest[OneToZeroOneEntity] {
  override def entity(tableName: String, alias: String, primaryKey: Keys, otherKeys: Keys, dataFields: List[FieldType[_]], children: List[_ <: ChildEntity]) = {
    OneToZeroOneEntity(tableName, alias, primaryKey, otherKeys, dataFields, children)
  }
}