package one.xingyi.core.orm

import java.io.ByteArrayOutputStream

import javax.sql.DataSource
import one.xingyi.core.closable.ClosableM
import one.xingyi.core.jdbc.{DatabaseSourceFixture, Jdbc, JdbcOps}
import one.xingyi.core.json.{JsonObject, JsonParser}

import scala.language.{higherKinds, implicitConversions}

trait SharedOrmFixture extends NumericKeyFixture {
  def checkStrategy(name: String, block: List[(OrmEntity, String)], expected: List[(OrmEntity, String)]): Unit = {
    val actual = block
    val fullDescription = List(name, actual.map(t => t._1.tableName.tableName + "->" + '"' + t._2 + '"').mkString(",\n"), "\n")
    val diffs = actual.zip(expected).collect { case (a, e) if a != e => s"Actual   ${a._1.tableName} -> ${a._2}\nExpected ${e._1.tableName} -> ${e._2}\n" }
    withClue((fullDescription ::: diffs).mkString("\n")) {
      actual.zip(expected).foreach { case (a, e) =>
        a._1.tableName shouldBe e._1.tableName
        a._2 shouldBe e._2
      }
      actual shouldBe expected
    }
  }

  val employerTable = TableName("Employer", "")
  val addressTable = TableName("Address", "")
  val phoneTable = TableName("Phone", "")
  //each person has a contact email, and the id of the email is the same as the person
  val emailTable = TableName("ContactEmail", "")
  val personTable = TableName("Person", "")

  val schemaForAddress = SchemaItemWithChildren("address", true, List[SchemaForTest](SchemaItem("Address/add")))
  val schemaForPhone = SchemaItemWithChildren("phone", true, List[SchemaForTest](SchemaItem("Phone/phoneNo")))

  val schemaForPerson: List[SchemaForTest] = {
    implicit def stringToSchemaForTest(s: String): SchemaForTest = SchemaItem(s)
    List[SchemaForTest](
      "Person/name",
      SchemaItemWithChildren("employer", false, List[SchemaForTest]("Employer/name")),
      schemaForAddress,
      schemaForPhone,
      SchemaItemWithChildren("email", false, List[SchemaForTest]("ContactEmail/email"))
    )
  }
  val numericKeysForPerson: NumericKeys[SchemaForTest] = NumericKeys(schemaForPerson)
}

abstract class SharedFastOrmTests[M[_] : ClosableM, J: JsonParser, DS <: DataSource] extends SharedOrmFixture with NumericKeyFixture with DatabaseSourceFixture[DS] with Jdbc {

  def setupPerson(ds: DataSource)(block: => Unit)(implicit jdbcOps: JdbcOps[DataSource], closableM: ClosableM[M]): Unit
  def main: MainEntity
  //  def mainUsingArrays: MainEntity

  implicit def maker: OrmMaker[Person]
  behavior of getClass.getSimpleName + " get data"

  behavior of getClass.getSimpleName + " " + classOf[FastReaderImpl[Person]].getSimpleName

  it should "allow the items to be read through the FastReader" in {
    val reader: FastReaderImpl[Person] = FastReader[Person](OrmBatchConfig(ds, 2))
    setupPerson(ds) {
      reader(main)(0) shouldBe List(Person("Phil", Employer("Employer1"), List(Address("Phils first address"), Address("Phils second address")), List(), "philsEmail"),
        Person("Bob", Employer("Employer2"), List(), List(), "bobsEmail"))
      reader(main)(1) shouldBe List(Person("Jill", Employer("Employer1"), List(Address("Jills first address")), List(), "jillsEmail"))
      reader(main)(2) shouldBe List()
    }
  }


  it should "allow the items to be read as a stream" in {

    setupPerson(ds) {
      main.stream[Person](OrmBatchConfig(ds, 2)).toList shouldBe
        List(Person("Phil", Employer("Employer1"), List(Address("Phils first address"), Address("Phils second address")), List(), "philsEmail"),
          Person("Bob", Employer("Employer2"), List(), List(), "bobsEmail"),
          Person("Jill", Employer("Employer1"), List(Address("Jills first address")), List(), "jillsEmail"))
    }
  }
  behavior of getClass.getSimpleName + " with ormFactory"

  it should "checkAssumptions and structure" in {
    checkNumericKeys(numericKeysForPerson)(
      """Person/name (),0 NoChildren
        |employer (),1 OneChild
        | Employer/name (1),0 NoChildren
        |address (),2 ManyChildren
        | Address/add (2),0 NoChildren
        |phone (),3 ManyChildren
        | Phone/phoneNo (3),0 NoChildren
        |email (),4 OneChild
        | ContactEmail/email (4),0 NoChildren""".stripMargin)
    val ar = numericKeysForPerson.makeAndSetupArray
    checkArray(numericKeysForPerson, ar)(
      """0  = null {Person/name}
        |1/OneChild
        |1.0  = null {Employer/name}
        |2/Many(0)
        |3/Many(0)
        |4/OneChild
        |4.0  = null {ContactEmail/email}""".stripMargin)

    val tablesAndFieldsAndPaths: TablesAndFieldsAndPaths = EntityAndPath(numericKeysForPerson)

    checkStrings(tablesAndFieldsAndPaths.prettyPrint.mkString("\n"),
      """Address
        |   0 add - (2) - 0
        |ContactEmail
        |   0 email - (4) - 0
        |Employer
        |   0 name - (1) - 0
        |Person
        |   0 name - () - 0
        |Phone
        |   0 phoneNo - (3) - 0
        |""".stripMargin)
  }

  lazy val tablesAndFieldsAndPaths: TablesAndFieldsAndPaths = EntityAndPath(numericKeysForPerson)
  def mainEntityForKeys: EntityAndFieldsAndPath[MainEntity]
  def addressEntity: EntityAndFieldsAndPath[OneToManyEntity]
  def phoneEntity: EntityAndFieldsAndPath[OneToManyEntity]
  lazy val ormFactory = tablesAndFieldsAndPaths.ormFactory(numericKeysForPerson)

  def mapForNext: Map[OneToManyEntity, NumericKey[_]] = Map(
    addressEntity.entity -> numericKeysForPerson.findForT(schemaForAddress).get,
    phoneEntity.entity -> numericKeysForPerson.findForT(schemaForPhone).get)


  it should "allow the items to be read using the numeric keys" in {
    //under development
    setupPerson(ds) {
      implicit val maker: OrmMakerForArrayAny[SchemaForTest] = ormFactory.ormMaker(mapForNext).asInstanceOf[OrmMakerForArrayAny[SchemaForTest]]
      maker.createdCount shouldBe 0
      val data = FastReader.getOneBlockOfDataFromDs(ds, mainEntityForKeys.entity, 2)(0)
      maker(mainEntityForKeys.entity)(data)
      maker.createdCount shouldBe 1
      val arrayForRow0 = mainEntityForKeys.entity.stream[Array[Any]](OrmBatchConfig(ds, 3)).toList
      maker.createdCount shouldBe 2
      checkArray(numericKeysForPerson, arrayForRow0(0))(
        """0  = name:Phil {Person/name}
          |1/OneChild
          |1.0  = name:Employer1 {Employer/name}
          |2/Many(2)
          |2[0].0  = add:Phils second address {Address/add}
          |2[1].0  = add:Phils first address {Address/add}
          |3/Many(0)
          |4/OneChild
          |4.0  = email:philsEmail {ContactEmail/email}""".stripMargin)
      checkArray(numericKeysForPerson, arrayForRow0(1))(
        """0  = name:Bob {Person/name}
          |1/OneChild
          |1.0  = name:Employer2 {Employer/name}
          |2/Many(0)
          |3/Many(0)
          |4/OneChild
          |4.0  = email:bobsEmail {ContactEmail/email}""".stripMargin)
      checkArray(numericKeysForPerson, arrayForRow0(2))(
        """0  = name:Jill {Person/name}
          |1/OneChild
          |1.0  = name:Employer1 {Employer/name}
          |2/Many(1)
          |2[0].0  = add:Jills first address {Address/add}
          |3/Many(0)
          |4/OneChild
          |4.0  = email:jillsEmail {ContactEmail/email}""".stripMargin)
      val stream = new ByteArrayOutputStream()
      maker.createdCount shouldBe 2
      mainEntityForKeys.entity.stream[Array[Any]](OrmBatchConfig(ds, 3)).foreach(numericKeysForPerson.putJson(_, stream))
      maker.createdCount shouldBe 3
      checkStrings(stream.toString(),
        """{"Person/name":"name:Phil","employer":{"Employer/name":"name:Employer1"},"address":[{"Address/add":"add:Phils first address"},{"Address/add":"add:Phils second address"}],"phone":[],"email":{"ContactEmail/email":"email:philsEmail"}}
          |{"Person/name":"name:Bob","employer":{"Employer/name":"name:Employer2"},"address":[],"phone":[],"email":{"ContactEmail/email":"email:bobsEmail"}}
          |{"Person/name":"name:Jill","employer":{"Employer/name":"name:Employer1"},"address":[{"Address/add":"add:Jills first address"}],"phone":[],"email":{"ContactEmail/email":"email:jillsEmail"}}""".stripMargin)
      maker.createdCount shouldBe 3
    }
  }



}
