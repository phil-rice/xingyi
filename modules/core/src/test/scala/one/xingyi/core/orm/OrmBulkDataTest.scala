package one.xingyi.core.orm

import java.io.ByteArrayOutputStream

import one.xingyi.core.closable.SimpleClosable
import one.xingyi.core.orm.SchemaMapKey._

import scala.language.{existentials, higherKinds}

trait OrmBulkDataFixture[M[_]] extends FastOrmFixture[M] {
  implicit val tableNameForManySchema = ArrayAliasFromMap[SchemaForTest](Map("address" -> address.alias, "phone" -> phone.alias))
}

class OrmBulkDataTest extends OrmBulkDataFixture[SimpleClosable] {
  implicit val linkPrefix: LinkPrefixFrom[String] = (c: String) => List(c)


  val entityToData: Map[OrmEntity, List[List[Any]]] = Map(
    address -> List(List("Phils first address", 3, 1), List("SomeOneElse address", 4, 2), List("Phils second address", 2, 1)),
    employer -> List(List("Employer1", 1), List("Employer2", 2)),
    email -> List(List("anotherEmail", 0), List("philsEmail", 1), List("bobsEmail", 2)),
    phone -> List(),
    main -> List(List("Phil", 1, 1), List("Bob", 2, 2)))

  lazy val mainBulkData = MainBulkData(main, entityToData)

  behavior of classOf[OrmBulkData[_]].getSimpleName + ".apply"

  it should "make a mainBulkData" in {
    mainBulkData.alias shouldBe main.alias
    mainBulkData.tableNameToData shouldBe Map(
      address.alias.table.name -> List(List("Phils first address", 3, 1), List("SomeOneElse address", 4, 2), List("Phils second address", 2, 1)),
      employer.alias.table .name-> List(List("Employer1", 1), List("Employer2", 2)),
      email.alias.table .name-> List(List("anotherEmail", 0), List("philsEmail", 1), List("bobsEmail", 2)),
      phone.alias.table.name -> List(),
      main.alias.table.name -> List(List("Phil", 1, 1), List("Bob", 2, 2)))
  }

  it should "make children" in {
    val List(employerBd, addressBd, phoneBd, emailBd) = mainBulkData.children
    employerBd.alias shouldBe employer.alias
    employerBd.tableNameToData shouldBe mainBulkData.tableNameToData
    employerBd.children shouldBe List()

    addressBd.alias shouldBe address.alias
    addressBd.children shouldBe List()
    addressBd.tableNameToData shouldBe mainBulkData.tableNameToData

    phoneBd.alias shouldBe phone.alias
    phoneBd.children shouldBe List()
    phoneBd.tableNameToData shouldBe mainBulkData.tableNameToData

    emailBd.alias shouldBe email.alias
    emailBd.children shouldBe List()
    emailBd.tableNameToData shouldBe mainBulkData.tableNameToData
  }

  it should "Make manyToOne with a idToIndex " in {
    val List(employerBd, addressBd, phoneBd, emailBd) = mainBulkData.children
    employerBd.alias shouldBe employer.alias
    employerBd.tableNameToData shouldBe mainBulkData.tableNameToData
    employerBd.children shouldBe List()
    employerBd.asInstanceOf[ManyToOneBulkData].idToIndex shouldBe Map(List(1) -> 0, List(2) -> 1)

  }
  it should "Make one to many with a parentIdToListOfIndexes " in {
    val List(employerBd, addressBd, phoneBd, emailBd) = mainBulkData.children

    addressBd.alias shouldBe address.alias
    addressBd.children shouldBe List()
    addressBd.asInstanceOf[OneToManyBulkData].parentIdToListOfIndexes shouldBe Map(List(1) -> List(0, 2), List(2) -> List(1))

  }

  it should "Make a sameId with an idToIndex" in {
    val List(employerBd, addressBd, phoneBd, emailBd) = mainBulkData.children

    emailBd.alias shouldBe email.alias
    emailBd.children shouldBe List()
    emailBd.asInstanceOf[SameIdBulkData].idToIndex shouldBe Map(List(0) -> 0, List(1) -> 1, List(2) -> 2)
  }

  behavior of classOf[BulkDataPointer].getSimpleName

  it should "make a BulkDataPointer" in {
    val pointer0 = mainBulkData.pointer(0)

    checkStrings(pointer0.prettyPrint(""),
      """Found(nth=0, bulkData=Person/P,row=Some(List(Phil, 1, 1)),children=
        |  Found(n=0,index=0,parentId=List(1),row=Some(List(Employer1, 1)),bulkData=Employer/E(Some(0)),noChildren
        |  Found(n=0,index=0,parentId=List(1),row=Some(List(Phils first address, 3, 1)),bulkData=Address/A(0,2),noChildren
        |  Null()
        |  Found(n=0,index=1,parentId=List(1),row=Some(List(philsEmail, 1)),bulkData=ContactEmail/CE(1),noChildren""".stripMargin)
  }

  it should "be able to iterate over children" in {
    val pointer0 = mainBulkData.pointer(0)
    println("size: " + pointer0.allPointers(addressAlias).size)
    val Seq(firstAddress, secondAddress) = pointer0.allPointers(addressAlias)
    checkStrings(firstAddress.prettyPrint(""),
      """Found(nth=0, bulkData=Person/P,row=Some(List(Phil, 1, 1)),children=
        |  Found(n=0,index=0,parentId=List(1),row=Some(List(Employer1, 1)),bulkData=Employer/E(Some(0)),noChildren
        |  Found(n=0,index=0,parentId=List(1),row=Some(List(Phils first address, 3, 1)),bulkData=Address/A(0,2),noChildren
        |  Null()
        |  Found(n=0,index=1,parentId=List(1),row=Some(List(philsEmail, 1)),bulkData=ContactEmail/CE(1),noChildren""".stripMargin)
    checkStrings(secondAddress.prettyPrint(""),
      """Found(nth=0, bulkData=Person/P,row=Some(List(Phil, 1, 1)),children=
        |  Found(n=0,index=0,parentId=List(1),row=Some(List(Employer1, 1)),bulkData=Employer/E(Some(0)),noChildren
        |  Found(n=1,index=2,parentId=List(1),row=Some(List(Phils second address, 2, 1)),bulkData=Address/A(0,2),noChildren
        |  Null()
        |  Found(n=0,index=1,parentId=List(1),row=Some(List(philsEmail, 1)),bulkData=ContactEmail/CE(1),noChildren""".stripMargin)

  }

  behavior of classOf[PartitionedSchema[SchemaForTest]].getSimpleName
  it should "be created" in {

    tableNameForManySchema(schemaForPerson) shouldBe None
    tableNameForManySchema(schemaForAddress) shouldBe Some(address.alias)
    val p = PartitionedSchema("someKey", schemaForPerson)
    p.simple.map(_.key) shouldBe List("Person/name")
    p.links.map(_.key) shouldBe List()
    p.arrays.map { case (alias, o) => alias.table.name + "->" + o.key } shouldBe List("Address->address", "Phone->phone")
    p.objects.map(_.key) shouldBe List("employer", "email")
  }

  behavior of classOf[WriteToJsonForSchema[SchemaForTest, String]].getSimpleName

  it should "create json" in {
    val stream = new ByteArrayOutputStream()
    val writer = new WriteToJsonForSchema[SchemaForTest, String]("someContext", stream)
    writer.toJson(mainBulkData.pointer(0), PartitionedSchema("person", schemaForPerson))
    checkStrings(stream.toString,
      """{"employer":{"Employer/name":"Employer1"},
        |"email":{"ContactEmail/email":"philsEmail"},
        |"address":[{"Address/add":"Phils first address"},{"Address/add":"Phils second address"}],
        |"phone":[],
        |"Person/name":"Phil"}""".stripMargin)
  }

  it should "create json even if a table referenced by the schema isn't in the data" in {
    val stream = new ByteArrayOutputStream()
    val writer = new WriteToJsonForSchema[SchemaForTest, String]("someContext", stream)

    val schemaWithTableNotFound = SchemaItemWithChildren("person", true, schemaListForPerson :+ SchemaItem[String]("NotIn/someField"))

    writer.toJson(mainBulkData.pointer(0), PartitionedSchema("person", schemaWithTableNotFound))

    checkStrings(stream.toString,
      """{"employer":{"Employer/name":"Employer1"},
        |"email":{"ContactEmail/email":"philsEmail"},
        |"address":[{"Address/add":"Phils first address"},
        |{"Address/add":"Phils second address"}],
        |"phone":[],
        |"Person/name":"Phil"}""".stripMargin)
  }

}


