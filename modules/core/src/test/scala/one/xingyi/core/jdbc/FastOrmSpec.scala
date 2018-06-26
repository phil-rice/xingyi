package one.xingyi.core.jdbc

import java.sql.ResultSet

import javax.sql.DataSource
import one.xingyi.core.closable.ClosableM
import one.xingyi.core.orm._

import scala.language.higherKinds
import one.xingyi.core.map.Maps._
trait FastOrmFixture {
  val address = OneToManyEntity("Address", "A", IntField("aid"), IntField("personid"), List(StringField("add")), List())
  val phone = OneToManyEntity("Phone", "Ph", IntField("aid"), IntField("personid"), List(StringField("phoneNo")), List())
  val main = MainEntity("Person", "P", IntField("pid"), List(StringField("name")), List(address, phone))

  case class Address(add: String)
  case class Phone(phoneNo: String)
  case class Person(name: String, address: List[Address], phones: List[Phone])


  implicit val maker: OrmMaker[Person] = { map: Map[OrmEntity, List[List[AnyRef]]] =>
    import OrmMaker._
    val aList = toMap(map(address))(implicit list => Address(str(0)))
    val phoneList = toMap(map(phone))(implicit list => Phone(str(0)))
    map(main).mapPf { case id :: name :: _ => Person(name.toString, aList.items(id), phoneList.items(id)) }
  }


  import Jdbc._
  def setupPerson[M[_] : ClosableM](ds: DataSource): Unit = {
    def execute = { s: String => executeSql(s) apply ds }
    def query = { s: String => getList(s) { rs: ResultSet => (1 to rs.getMetaData.getColumnCount).toList.map(rs.getObject) } apply ds }

    OrmStrategies.dropTables.map(execute).walk(main)
    OrmStrategies.createTables.map(execute).walk(main)
    executeSql(s"""insert into  Person (pid, name ) values (1, 'Phil');""") apply ds
    executeSql(s"""insert into  Address (aid, personid, add ) values (1, 1, 'Phils first address');""") apply ds
    executeSql(s"""insert into  Address (aid, personid, add ) values (2, 1, 'Phils second address');""") apply ds
    executeSql(s"""insert into  Person (pid, name ) values (2, 'Bob');""") apply ds
    executeSql(s"""insert into  Person (pid, name ) values (3, 'Jill');""") apply ds
    executeSql(s"""insert into  Address (aid, personid, add ) values (3, 3, 'Jills first address');""") apply ds
    OrmStrategies.dropTempTables.map(execute).walk(main)
  }

}

abstract class AbstractFastOrmSpec[M[_] : ClosableM, DS <: DataSource] extends DatabaseSourceFixture[DS] with FastOrmFixture with Jdbc {


  behavior of "FastOrm"

  it should "make drop table sql" in {
    def printIt = { s: Any => println(s) }
    OrmStrategies.dropTables.walk(main) shouldBe List(
      main -> "drop table if exists Person",
      address -> "drop table if exists Address",
      phone -> "drop table if exists Phone"
    )
  }

  it should "make create table sql" in {
    OrmStrategies.createTables.walk(main) shouldBe List(
      main -> "create table Person (pid integer,name varchar(255))",
      address -> "create table Address (aid integer,personid integer,add varchar(255))",
      phone -> "create table Phone (aid integer,personid integer,phoneNo varchar(255))"
    )
  }

  it should "make dropTempTables sql" in {
    OrmStrategies.dropTempTables.walk(main) shouldBe List(
      main -> "drop table if exists temp_Person",
      address -> "drop table if exists temp_Address",
      phone -> "drop table if exists temp_Phone"
    )
  }

  it should "make createTempTables sql" in {
    OrmStrategies.createTempTables(BatchDetails(1000, 3)).walk(main) shouldBe List(
      main -> "create temporary table temp_Person as select P.pid, P.name from Person P limit 1000 offset 3000",
      address -> "create temporary table temp_Address as select A.personid, A.aid, A.add from temp_Person P,Address A where P.pid = A.personid",
      phone -> "create temporary table temp_Phone as select Ph.personid, Ph.aid, Ph.phoneNo from temp_Person P,Phone Ph where P.pid = Ph.personid"
    )
  }

  it should "make drainTempTables sql" in {
    OrmStrategies.drainTempTables.walk(main) shouldBe List(
      main -> "select * from temp_Person",
      address -> "select * from temp_Address",
      phone -> "select * from temp_Phone"
    )
  }

  behavior of classOf[FastReaderImpl[Person]].getSimpleName

  it should "allow the items to be read" in {
    val reader = new FastReaderImpl[Person](OrmBatchConfig(ds, 2))
    reader(main).toList shouldBe List(Person("Phil", List(Address("Phils first address"), Address("Phils second address")), List()), Person("Bob", List(), List()), Person("Jill", List(Address("Jills first address")), List()))
  }
}
