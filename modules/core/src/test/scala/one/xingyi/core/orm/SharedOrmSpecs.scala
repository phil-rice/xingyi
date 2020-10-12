package one.xingyi.core.orm

import java.io.ByteArrayOutputStream

import javax.sql.DataSource
import one.xingyi.core.closable.ClosableM
import one.xingyi.core.jdbc.{DatabaseSourceFixture, Jdbc, JdbcOps}
import one.xingyi.core.json.JsonParser
import org.scalatest.matchers.should.Matchers

import scala.language.{higherKinds, implicitConversions}

trait CheckStrategyFixture extends Matchers {
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
}

trait SetupDatabaseForOrmFixture[DS <: DataSource] {
  def ds: DS
  def setup[M[_] : ClosableM](ds: DS, main: MainEntity)(block: => Unit)(implicit jdbcOps: JdbcOps[DataSource]): Unit = {
    def executeIt(implicit jdbcOps: JdbcOps[DataSource]): String => Boolean = { s: String => jdbcOps.executeSql(s) apply ds }

    OrmStrategies.dropTables.map(executeIt).walk(main)
    OrmStrategies.createTables.map(executeIt).walk(main)
    OrmStrategies.dropTempTables.map(executeIt).walk(main)
    try {
      block
    } finally {
      OrmStrategies.dropTempTables.map(executeIt).walk(main)
      OrmStrategies.dropTables.map(executeIt).walk(main)
    }
  }

}

trait SharedOrmFixture extends OrmKeyFixture {

  val employerTable = TableName("Employer", "")
  val addressTable = TableName("Address", "")
  val phoneTable = TableName("Phone", "")
  //each person has a contact email, and the id of the email is the same as the person
  val emailTable = TableName("ContactEmail", "")
  val personTable = TableName("Person", "")

  val schemaForAddress = SchemaItemWithChildren("address", true, List[SchemaForTest[_]](SchemaItem[String]("Address/add")))
  val schemaForPhone = SchemaItemWithChildren("phone", true, List[SchemaForTest[_]](SchemaItem[String]("Phone/phoneNo")))

  val schemaListForPerson: List[SchemaForTest[_]] = {
    implicit def stringToSchemaForTest(s: String): SchemaForTest[_] = SchemaItem[String](s)
    List[SchemaForTest[_]](
      "Person/name",
      SchemaItemWithChildren("employer", false, List[SchemaForTest[_]]("Employer/name")),
      schemaForAddress,
      schemaForPhone,
      SchemaItemWithChildren("email", false, List[SchemaForTest[_]]("ContactEmail/email"))
    )
  }
  val schemaForPerson=SchemaItemWithChildren("person", true, schemaListForPerson)
  val numericKeysForPerson: OrmKeys[SchemaForTest] = OrmKeys.fromList(schemaListForPerson)
}

abstract class SharedFastOrmTests[M[_] : ClosableM, J: JsonParser, DS <: DataSource] extends SharedOrmFixture with OrmKeyFixture with DatabaseSourceFixture[DS] with Jdbc {

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
    checkKeys(numericKeysForPerson)(
      """employer:O(),0 OneChild
        | Employer/name:S(0),0 NoChildren
        |address:O(),1 ManyChildren
        | Address/add:S(1),0 NoChildren
        |phone:O(),2 ManyChildren
        | Phone/phoneNo:S(2),0 NoChildren
        |email:O(),3 OneChild
        | ContactEmail/email:S(3),0 NoChildren
        |Person/name:S(),4 NoChildren""".stripMargin)
    val ar = numericKeysForPerson.makeAndSetupArray
    checkArray(numericKeysForPerson, ar)(
      """0/OneChild
        |0.0  = null {Employer/name}
        |1/Many(0)
        |2/Many(0)
        |3/OneChild
        |3.0  = null {ContactEmail/email}
        |4  = null {Person/name}""".stripMargin)

    val tablesAndFieldsAndPaths: TablesAndFieldsAndPaths = EntityAndPath(numericKeysForPerson)

    checkStrings(tablesAndFieldsAndPaths.prettyPrint.mkString("\n"),
      """Address
        |   0 add/varchar(255) - (1) - 0
        |ContactEmail
        |   0 email/varchar(255) - (3) - 0
        |Employer
        |   0 name/varchar(255) - (0) - 0
        |Person
        |   0 name/varchar(255) - () - 4
        |Phone
        |   0 phoneNo/varchar(255) - (2) - 0""".stripMargin)
  }

  lazy val tablesAndFieldsAndPaths: TablesAndFieldsAndPaths = EntityAndPath(numericKeysForPerson)
  def mainEntityForKeys: EntityAndFieldsAndPath[MainEntity]
  def addressEntity: EntityAndFieldsAndPath[OneToManyEntity]
  def phoneEntity: EntityAndFieldsAndPath[OneToManyEntity]
  lazy val ormFactory = tablesAndFieldsAndPaths.ormFactory(numericKeysForPerson)

  def mapForNext: Map[OneToManyEntity, OrmKey[SchemaForTest, _]] = Map(
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
        """0/OneChild
          |0.0  = name:Employer1 {Employer/name}
          |1/Many(2)
          |1[0].0  = add:Phils second address {Address/add}
          |1[1].0  = add:Phils first address {Address/add}
          |2/Many(0)
          |3/OneChild
          |3.0  = email:philsEmail {ContactEmail/email}
          |4  = name:Phil {Person/name}""".stripMargin)
      checkArray(numericKeysForPerson, arrayForRow0(1))(
        """0/OneChild
          |0.0  = name:Employer2 {Employer/name}
          |1/Many(0)
          |2/Many(0)
          |3/OneChild
          |3.0  = email:bobsEmail {ContactEmail/email}
          |4  = name:Bob {Person/name}""".stripMargin)
      checkArray(numericKeysForPerson, arrayForRow0(2))(
        """0/OneChild
          |0.0  = name:Employer1 {Employer/name}
          |1/Many(1)
          |1[0].0  = add:Jills first address {Address/add}
          |2/Many(0)
          |3/OneChild
          |3.0  = email:jillsEmail {ContactEmail/email}
          |4  = name:Jill {Person/name}""".stripMargin)
      val stream = new ByteArrayOutputStream()
      maker.createdCount shouldBe 2
      mainEntityForKeys.entity.stream[Array[Any]](OrmBatchConfig(ds, 3)).foreach((ar: Array[Any]) => numericKeysForPerson.writeJsonPrimitive("someContext", ar, stream))
      maker.createdCount shouldBe 3
      checkStrings(stream.toString(),
        """{"employer":{"Employer/name":"name:Employer1"},"address":[{"Address/add":"add:Phils first address"},{"Address/add":"add:Phils second address"}],"phone":[],"email":{"ContactEmail/email":"email:philsEmail"},"Person/name":"name:Phil"}
                    {"employer":{"Employer/name":"name:Employer2"},"address":[],"phone":[],"email":{"ContactEmail/email":"email:bobsEmail"},"Person/name":"name:Bob"}
                    {"employer":{"Employer/name":"name:Employer1"},"address":[{"Address/add":"add:Jills first address"}],"phone":[],"email":{"ContactEmail/email":"email:jillsEmail"},"Person/name":"name:Jill"}""")
      maker.createdCount shouldBe 3
    }
  }

  it should "allow the items to be read using the numeric keys with the ormData" in {
    //under development
    setupPerson(ds) {
      implicit val maker: OrmMakerForArrayAnyUsingOrmData[SchemaForTest] = ormFactory.ormDataMaker(mapForNext).asInstanceOf[OrmMakerForArrayAnyUsingOrmData[SchemaForTest]]
      maker.createdCount shouldBe 0
      val data = FastReader.getOneBlockOfDataFromDs(ds, mainEntityForKeys.entity, 2)(0)
      maker(mainEntityForKeys.entity)(data)
      maker.createdCount shouldBe 1
      val arrayForRow0 = mainEntityForKeys.entity.stream[Array[Any]](OrmBatchConfig(ds, 3)).toList
      maker.createdCount shouldBe 2
      checkArray(numericKeysForPerson, arrayForRow0(0))(
        """0/OneChild
          |0.0  = name:Employer1 {Employer/name}
          |1/Many(2)
          |1[0].0  = add:Phils second address {Address/add}
          |1[1].0  = add:Phils first address {Address/add}
          |2/Many(0)
          |3/OneChild
          |3.0  = email:philsEmail {ContactEmail/email}
          |4  = name:Phil {Person/name}""".stripMargin)
      checkArray(numericKeysForPerson, arrayForRow0(1))(
        """0/OneChild
          |0.0  = name:Employer2 {Employer/name}
          |1/Many(0)
          |2/Many(0)
          |3/OneChild
          |3.0  = email:bobsEmail {ContactEmail/email}
          |4  = name:Bob {Person/name}""".stripMargin)
      checkArray(numericKeysForPerson, arrayForRow0(2))(
        """0/OneChild
          |0.0  = name:Employer1 {Employer/name}
          |1/Many(1)
          |1[0].0  = add:Jills first address {Address/add}
          |2/Many(0)
          |3/OneChild
          |3.0  = email:jillsEmail {ContactEmail/email}
          |4  = name:Jill {Person/name}""".stripMargin)
      val stream = new ByteArrayOutputStream()
      maker.createdCount shouldBe 2
      mainEntityForKeys.entity.stream[Array[Any]](OrmBatchConfig(ds, 3)).foreach((ar: Array[Any]) => numericKeysForPerson.writeJsonPrimitive("someContext", ar, stream))
      maker.createdCount shouldBe 3
      checkStrings(stream.toString(),
        """{"employer":{"Employer/name":"name:Employer1"},"address":[{"Address/add":"add:Phils first address"},{"Address/add":"add:Phils second address"}],"phone":[],"email":{"ContactEmail/email":"email:philsEmail"},"Person/name":"name:Phil"}
          |{"employer":{"Employer/name":"name:Employer2"},"address":[],"phone":[],"email":{"ContactEmail/email":"email:bobsEmail"},"Person/name":"name:Bob"}
          |{"employer":{"Employer/name":"name:Employer1"},"address":[{"Address/add":"add:Jills first address"}],"phone":[],"email":{"ContactEmail/email":"email:jillsEmail"},"Person/name":"name:Jill"}""".stripMargin)
      maker.createdCount shouldBe 3
    }
  }


}
