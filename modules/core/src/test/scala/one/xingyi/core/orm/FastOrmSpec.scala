/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.orm

import java.sql.ResultSet

import javax.sql.DataSource
import one.xingyi.core.closable.ClosableM
import one.xingyi.core.jdbc.{DatabaseSourceFixture, Jdbc, JdbcOps}
import one.xingyi.core.json.{JsonObject, JsonParser}
import one.xingyi.core.map.Maps._
import one.xingyi.core.orm.FieldType.{int, string}
import one.xingyi.core.strings.Strings

import scala.language.higherKinds

trait OrmFixture {
  val employer = ManyToOneEntity("Employer", "E", int("eid"), int("employerid"), List(string("name")), List())
  val address = OneToManyEntity("Address", "A", int("aid"), int("personid"), List(string("add")), List())
  val phone = OneToManyEntity("Phone", "Ph", int("aid"), int("personid"), List(string("phoneNo")), List())
  //each person has a contact email, and the id of the email is the same as the person
  val email = SameIdEntity("ContactEmail", "E", int("eid"), List(string("email")), List())
  val main = MainEntity("Person", "P", int("pid"), List(string("name")), List(employer, address, phone, email))

  case class Employer(name: String)
  case class Address(add: String)
  case class Phone(phoneNo: String)
  case class Person(name: String, employer: Employer, address: List[Address], phones: List[Phone], email: String)

}

trait FastOrmFixture extends OrmFixture {
  implicit val maker: OrmMaker[Person] = { main =>
    data: Map[OrmEntity, List[List[AnyRef]]] =>
      import OrmMaker._
      val eList = toMapForManyToOneAndSameId(data(employer))(implicit list => Employer(str(0)))
      val aList = toMapForOneToMany(data(address))(implicit list => Address(str(0)))
      val phoneList = toMapForOneToMany(data(phone))(implicit list => Phone(str(0)))
      val emailList = toMapForManyToOneAndSameId(data(email))(implicit list => str(0))

      data(main).mapPf { case id :: employerId :: name :: _ =>
        Person(name.toString, eList(employerId), aList.items(id), phoneList.items(id), emailList(id))
      }
  }


  import Jdbc._

  def setupPerson[M[_] : ClosableM](ds: DataSource)(block: => Unit)(implicit jdbcOps: JdbcOps[DataSource]): Unit = {
    import jdbcOps._
    def execute = { s: String => executeSql(s) apply ds }
    def query = { s: String => getList(s) { rs: ResultSet => (1 to rs.getMetaData.getColumnCount).toList.map(rs.getObject) } apply ds }

    OrmStrategies.dropTables.map(execute).walk(main)
    OrmStrategies.createTables.map(execute).walk(main)
    executeSql(s"""insert into  Employer (eid, name ) values (1, 'Employer1');""") apply ds
    executeSql(s"""insert into  Employer (eid, name ) values (2, 'Employer2');""") apply ds
    executeSql(s"""insert into  Person (pid,employerid, name ) values (1, 1,'Phil');""") apply ds
    executeSql(s"""insert into  ContactEmail (eid,email ) values (1, 'philsEmail');""") apply ds
    executeSql(s"""insert into  Address (aid, personid, add ) values (2, 1, 'Phils first address');""") apply ds
    executeSql(s"""insert into  Address (aid, personid, add ) values (3, 1, 'Phils second address');""") apply ds
    executeSql(s"""insert into  Person (pid, employerid,name ) values (2, 2,'Bob');""") apply ds
    executeSql(s"""insert into  ContactEmail (eid,email ) values (2, 'bobsEmail');""") apply ds
    executeSql(s"""insert into  Person (pid, employerid,name ) values (3, 1,'Jill');""") apply ds
    executeSql(s"""insert into  ContactEmail (eid,email ) values (3, 'jillsEmail');""") apply ds
    executeSql(s"""insert into  Address (aid, personid, add ) values (4, 3, 'Jills first address');""") apply ds
    OrmStrategies.dropTempTables.map(execute).walk(main)
    try {
      block
    } finally {
      OrmStrategies.dropTables.map(execute).walk(main)
    }
  }
}

abstract class AbstractFastOrmSpec[M[_] : ClosableM, J: JsonParser, DS <: DataSource] extends DatabaseSourceFixture[DS] with FastOrmFixture with Jdbc {


  behavior of "FastOrm"

  it should "make drop table sql" in {
    def printIt = { s: Any => println(s) }
    OrmStrategies.dropTables.walk(main) shouldBe List(
      main -> "drop table if exists Person",
      employer -> "drop table if exists Employer",
      address -> "drop table if exists Address",
      phone -> "drop table if exists Phone",
      email -> "drop table if exists ContactEmail"
    )
  }

  it should "make create table sql" in {
    OrmStrategies.createTables.walk(main) shouldBe List(
      main -> "create table Person (pid integer,employerid integer,name varchar(255))",
      employer -> "create table Employer (eid integer,name varchar(255))",
      address -> "create table Address (aid integer,personid integer,add varchar(255))",
      phone -> "create table Phone (aid integer,personid integer,phoneNo varchar(255))",
      email -> "create table ContactEmail (eid integer,email varchar(255))"
    )
  }

  it should "make dropTempTables sql" in {
    OrmStrategies.dropTempTables.walk(main) shouldBe List(
      main -> "drop table if exists temp_Person",
      employer -> "drop table if exists temp_Employer",
      address -> "drop table if exists temp_Address",
      phone -> "drop table if exists temp_Phone",
      email -> "drop table if exists temp_ContactEmail"
    )
  }

  it should "make createTempTables sql" in {
    OrmStrategies.createTempTables(BatchDetails(1000, 3)).walk(main) shouldBe List(
      main -> "create temporary table temp_Person as select P.pid, P.employerid, P.name from Person P limit 1000 offset 3000",
      employer -> "create temporary table temp_Employer as select DISTINCT  E.eid, E.name from temp_Person P,Employer E where P.employerid = E.eid",
      address -> "create temporary table temp_Address as select A.personid, A.aid, A.add from temp_Person P,Address A where P.pid = A.personid",
      phone -> "create temporary table temp_Phone as select Ph.personid, Ph.aid, Ph.phoneNo from temp_Person P,Phone Ph where P.pid = Ph.personid",
      email -> "create temporary table temp_ContactEmail as select DISTINCT  E.eid, E.email from temp_Person P,ContactEmail E where P.pid = E.eid"
    )
  }

  it should "make drainTempTables sql" in {
    OrmStrategies.drainTempTables.walk(main) shouldBe List(
      main -> "select * from temp_Person",
      employer -> "select * from temp_Employer",
      address -> "select * from temp_Address",
      phone -> "select * from temp_Phone",
      email -> "select * from temp_ContactEmail"
    )
  }

  it should "have a pretty print" in {
    Strings.removeWhiteSpace(main.prettyPrint("")) shouldBe Strings.removeWhiteSpace(
      """MainEntity(Person, id=pid, childrenAdded=employerid, data=name){
        |  ManyToOne(Employer, id=eid, idInParent=employerid data=name)
        |  OneToMany(Address, id=aid, parent=personid data=add)
        |  OneToMany(Phone, id=aid, parent=personid data=phoneNo)
        |  SameId(ContactEmail, id=eid, data=email)
        |}""".stripMargin)
  }

  behavior of classOf[FastReaderImpl[Person]].getSimpleName

  it should "allow the items to be read through the FastReader" in {
    val reader: FastReaderImpl[Person] = FastReader(OrmBatchConfig(ds, 2))
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
  behavior of "OrmMakerForJson"

  it should "allow the items to be read as a stream of json" ignore {
    //under development
    setupPerson(ds) {
      main.stream[JsonObject](OrmBatchConfig(ds, 2)).toList shouldBe ""
    }
  }

}

