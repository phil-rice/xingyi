package one.xingyi.core.orm
import one.xingyi.core.UtilsSpec

trait EntityFixture {
  val personIdField = IntField("id")
  val nameField = StringField("name")

  val addressIdField = IntField("id")
  val addressPersonIdField = IntField("personId")
  val line1Field = StringField("line1")
  val line2Field = StringField("line2")


  val phoneIdField = IntField("id")
  val phonePersonIdField = IntField("personId")
  val manufacturerField = StringField("manufacturer")

  val phoneDetailsIdField = IntField("id")
  val purposeField = StringField("purpose")
  val telField = StringField("tel")

  val addressEntity = OneToManyEntity("address", "a", addressIdField, addressPersonIdField, List(line1Field, line2Field), List())
  val phoneDetailsEntity = OneToManyEntity("phonedetails", "pd", phoneDetailsIdField, phonePersonIdField, List(purposeField, telField), List())
  val phoneEntity = OneToManyEntity("phone", "ph", phoneIdField, phonePersonIdField, List(manufacturerField), List(phoneDetailsEntity))
  val mainEntity = MainEntity("person", "p", personIdField, List(nameField), List(addressEntity,phoneEntity))
}

class EntitySpec extends UtilsSpec with EntityFixture {

  behavior of "MainEntity"

  it should "have a fieldsForCreate which is the primary key and the data fields" in {
    mainEntity.fieldsForCreate shouldBe List(personIdField, nameField)
  }

  behavior of "OneToManyEntity"
  it should "have a fieldsForCreate which is the primary key the parent field and the data fields" in {
    addressEntity.fieldsForCreate shouldBe List(addressIdField, addressPersonIdField, line1Field, line2Field)
    phoneEntity.fieldsForCreate shouldBe List(phoneIdField, phonePersonIdField, manufacturerField)
    phoneDetailsEntity.fieldsForCreate shouldBe List(phoneDetailsIdField, phonePersonIdField, purposeField, telField)
  }
}
