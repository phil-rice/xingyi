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

  val employerAlias = Alias("Employer", alias="E")
  val addressAlias = Alias("Address",  alias="A")
  val phoneAlias = Alias("Phone",  alias="Ph")
  //each person has a contact email, and the id of the email is the same as the person
  val emailAlias = Alias("ContactEmail",  alias="E")
  val personAlias = Alias("Person",  alias="P")

  val schemaForAddress = SchemaItemWithChildren("address", true, List[SchemaForTest[_]](SchemaItem[String]("Address/add")))
  val schemaForPhone = SchemaItemWithChildren("phone", true, List[SchemaForTest[_]](SchemaItem[String]("Phone/phoneNo")))

  val schemaForEmployer = SchemaItemWithChildren("employer", false, List[SchemaForTest[_]](SchemaItem[String]("Employer/name")))
  val schemaForEmail = SchemaItemWithChildren("email", false, List[SchemaForTest[_]](SchemaItem[String]("ContactEmail/email")))
  val schemaListForPerson: List[SchemaForTest[_]] = {
    List[SchemaForTest[_]](
      SchemaItem[String]("Person/name"),
      schemaForEmployer,
      schemaForAddress,
      schemaForPhone,
      schemaForEmail
    )
  }
  val schemaForPerson = SchemaItemWithChildren("person", true, schemaListForPerson)
  val arrayTableNameForPerson = ArrayAliasFromMap[SchemaForTest](Map(schemaForAddress.key -> addressAlias, schemaForPhone.key -> phoneAlias))

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

  def mainEntity: MainEntity
  def addressEntity: OneToManyEntity
  def phoneEntity: OneToManyEntity
  lazy val ormFactory = OrmFactory[String, SchemaForTest](schemaForPerson)


}
