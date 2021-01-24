package one.xingyi.core.orm

class PartitionedSchemaTest extends SharedOrmFixture {

  behavior of classOf[IsSimpleFieldFilter[SchemaForTest]].getSimpleName
  it should "have a default that works for SchemaForTest" in {
    val filter = implicitly[IsSimpleFieldFilter[SchemaForTest]]
    filter(schemaForAddress) shouldBe false
    filter(schemaForPerson) shouldBe false
    filter(schemaForEmail) shouldBe false
    filter(SchemaItem[String]("someKey")) shouldBe true
  }

  behavior of classOf[IsLinkFieldFilter[SchemaForTest]].getSimpleName
  it should "This is just checking that the test fixture looks at the key starting with c, and no children" in {
    val filter = implicitly[IsLinkFieldFilter[SchemaForTest]]
    filter(schemaForAddress) shouldBe false
    filter(schemaForPerson) shouldBe false
    filter(schemaForEmail) shouldBe false
    filter(SchemaItem[String]("someKey")) shouldBe false
    filter(SchemaItem[String]("c")) shouldBe true
    filter(SchemaItemWithChildren("c", false, List())) shouldBe true
    filter(SchemaItemWithChildren("c", false, List(schemaForEmail))) shouldBe false
    filter(SchemaItemWithChildren("c", true, List(schemaForEmail))) shouldBe false
  }
  behavior of classOf[ArrayAlias[SchemaForTest]].getSimpleName

  it should "Checking the test fixture " in {
    implicit val arrayTableName = arrayTableNameForPerson
    val filter = implicitly[ArrayAlias[SchemaForTest]]
    filter(schemaForAddress) shouldBe Some(addressAlias)
    filter(schemaForPhone) shouldBe Some(phoneAlias)
    filter(schemaForPerson) shouldBe None
    filter(schemaForEmail) shouldBe None
    filter(SchemaItem[String]("someKey")) shouldBe None
  }

  behavior of classOf[PartitionedSchema[SchemaForTest]].getSimpleName

  it should "partition schemas for Person" in {
    implicit val arrayTableName = arrayTableNameForPerson
    val PartitionedSchema("someKey", links, simple, objects, arrays) = PartitionedSchema[SchemaForTest]("someKey", schemaForPerson)
    links shouldBe List()
    objects shouldBe List(
      PartitionedSchema[SchemaForTest](schemaForEmployer.key, schemaForEmployer),
      PartitionedSchema[SchemaForTest](schemaForEmail.key, schemaForEmail))
    arrays shouldBe List(
      (addressAlias,PartitionedSchema[SchemaForTest](schemaForAddress.key, schemaForAddress)),
      (phoneAlias,PartitionedSchema[SchemaForTest](schemaForPhone.key, schemaForPhone)))
  }
}
