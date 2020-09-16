/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.orm

import java.sql.ResultSet

import javax.sql.DataSource
import one.xingyi.core.closable.ClosableM
import one.xingyi.core.jdbc.{DatabaseSourceFixture, Jdbc, JdbcOps}
import one.xingyi.core.json.{JsonObject, JsonParser}
import one.xingyi.core.map.Maps._
import one.xingyi.core.orm.FieldType.{int, string}
import one.xingyi.core.orm.OrmMaker.str
import one.xingyi.core.strings.Strings

import scala.language.{higherKinds, implicitConversions}

trait OrmWithMultipleKeysFixture {

  val employer = ManyToOneEntity("Employer", "E", Keys("eid1:int,eid2:int"), Keys("employerid1:int,employerid2:int"), List(string("name")), List())
  val address = OneToManyEntity("Address", "A", Keys("aid1:int,aid2:int"), Keys("personid1:int,personid2:int"), List(string("add")), List())
  val phone = OneToManyEntity("Phone", "Ph", Keys("phid"), Keys("personid1:int,personid2:int"), List(string("phoneNo")), List())
  //each person has a contact email, and the id of the email is the same as the person
  val email = SameIdEntity("ContactEmail", "E", Keys("eid1:int,eid2:int"), List(string("email")), List())
  val main = MainEntity("Person", "P", Keys("pid1:int,pid2:int"), List(string("name")), List(employer, address, phone, email))

  case class Employer(name: String)
  case class Address(add: String)
  case class Phone(phoneNo: String)
  case class Person(name: String, employer: Employer, address: List[Address], phones: List[Phone], email: String)

}

trait FastWithMultipleKeysOrmFixture extends OrmWithMultipleKeysFixture {

  implicit val maker: OrmMaker[Person] = { main =>
    data: Map[OrmEntity, List[List[AnyRef]]] =>
      val eMap = employer.toMap(data, implicit list => Employer(str(employer.dataIndex)))
      val aMap = address.toOneToManyMap(data, implicit list => Address(str(address.dataIndex)))
      val phoneMap = phone.toOneToManyMap(data, implicit list => Phone(str(phone.dataIndex)))
      val emailMap = email.toMap(data, implicit list => str(email.dataIndex))
      data(main).map { implicit oneRow =>
        Person(str(main.dataIndex),
          employer.getData(main, eMap, oneRow),
          address.getData(main, aMap, oneRow),
          phone.getData(main, phoneMap, oneRow),
          email.getData(main, emailMap, oneRow))
      }.toStream
  }

  def setupPerson[M[_] : ClosableM](ds: DataSource)(block: => Unit)(implicit jdbcOps: JdbcOps[DataSource]): Unit = {
    import jdbcOps._
    def execute = { s: String => executeSql(s) apply ds }
    def query = { s: String => getList(s) { rs: ResultSet => (1 to rs.getMetaData.getColumnCount).toList.map(rs.getObject) } apply ds }

    OrmStrategies.dropTables.map(execute).walk(main)
    OrmStrategies.createTables.map(execute).walk(main)
    executeSql(s"""insert into  Employer (eid1,eid2, name ) values (1,10, 'Employer1');""") apply ds
    executeSql(s"""insert into  Employer (eid1,eid2, name ) values (2,20, 'Employer2');""") apply ds
    executeSql(s"""insert into  Person (pid1, pid2, employerid1, employerid2, name ) values (1,10, 1,10,'Phil');""") apply ds
    executeSql(s"""insert into  ContactEmail (eid1,eid2,email ) values (1, 10, 'philsEmail');""") apply ds
    executeSql(s"""insert into  Address (aid1,aid2, personid1, personid2, add ) values (2,20, 1,10, 'Phils first address');""") apply ds
    executeSql(s"""insert into  Address (aid1,aid2, personid1, personid2, add  ) values (3,30, 1,10, 'Phils second address');""") apply ds
    executeSql(s"""insert into  Person (pid1, pid2, employerid1, employerid2, name) values (2,20, 2,20,'Bob');""") apply ds
    executeSql(s"""insert into  ContactEmail (eid1,eid2,email ) values (2,20, 'bobsEmail');""") apply ds
    executeSql(s"""insert into  Person (pid1, pid2, employerid1, employerid2, name ) values (3,30, 1,10,'Jill');""") apply ds
    executeSql(s"""insert into  ContactEmail (eid1,eid2,email  ) values (3,30, 'jillsEmail');""") apply ds
    executeSql(s"""insert into  Address (aid1,aid2, personid1, personid2, add ) values (4,40, 3,30, 'Jills first address');""") apply ds
    OrmStrategies.dropTempTables.map(execute).walk(main)
    try {
      block
    } finally {
      OrmStrategies.dropTables.map(execute).walk(main)
    }
  }
}

abstract class AbstractWithMultipleKeysFastOrmSpec[M[_] : ClosableM, J: JsonParser, DS <: DataSource] extends DatabaseSourceFixture[DS] with FastWithMultipleKeysOrmFixture with Jdbc {


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
    //    OrmStrategies.createTables.walk(main).foreach(e => println(s"${e._1.tableName}  ${e._2}"))
    OrmStrategies.createTables.walk(main) shouldBe List(
      main -> "create table Person (pid1 integer,pid2 integer,employerid1 integer,employerid2 integer,name varchar(255))",
      employer -> "create table Employer (eid1 integer,eid2 integer,name varchar(255))",
      address -> "create table Address (aid1 integer,aid2 integer,personid1 integer,personid2 integer,add varchar(255))",
      phone -> "create table Phone (phid varchar(255),personid1 integer,personid2 integer,phoneNo varchar(255))",
      email -> "create table ContactEmail (eid1 integer,eid2 integer,email varchar(255))"
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
    val details = BatchDetails(1000, 3)
    //    OrmStrategies.createTempTables(details).walk(main).foreach(e => println(s"${e._1.tableName}  ${e._2}"))
    OrmStrategies.createTempTables(details).walk(main) shouldBe List(
      main -> "create temporary table temp_Person as select P.pid1, P.pid2, P.employerid1, P.employerid2, P.name from Person P limit 1000 offset 3000",
      employer -> "create temporary table temp_Employer as select DISTINCT  E.eid1, E.eid2, E.name from temp_Person P,Employer E where P.employerid1 = E.eid1 and P.employerid2 = E.eid2",
      address -> "create temporary table temp_Address as select A.personid1, A.personid2, A.aid1, A.aid2, A.add from temp_Person P,Address A where P.pid1 = A.personid1 and P.pid2 = A.personid2",
      phone -> "create temporary table temp_Phone as select Ph.personid1, Ph.personid2, Ph.phid, Ph.phoneNo from temp_Person P,Phone Ph where P.pid1 = Ph.personid1 and P.pid2 = Ph.personid2",
      email -> "create temporary table temp_ContactEmail as select DISTINCT  E.eid1, E.eid2, E.email from temp_Person P,ContactEmail E where P.pid1 = E.eid1 and P.pid2 = E.eid2"
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
    println(main.prettyPrint(""))
    Strings.removeWhiteSpace(main.prettyPrint("")) shouldBe Strings.removeWhiteSpace(
      """MainEntity(Person, id=pid1,pid2, childrenAdded=employerid1,employerid2, data=name){
        |  ManyToOne(Employer, id=eid1,eid2, idInParent=employerid1,employerid2 data=name)
        |  OneToMany(Address, id=aid1,aid2, parent=personid1,personid2 data=add)
        |  OneToMany(Phone, id=phid, parent=personid1,personid2 data=phoneNo)
        |  SameId(ContactEmail, id=eid1,eid2, data=email)
        |}""".stripMargin)
  }

  it should "allow the id used to select it to be extracted from the onerow of data of the parent for " in {
    //documenting assumptions
    main.fieldsAddedByChildren.map(_.name) shouldBe List("employerid1", "employerid2")
    main.fieldsForCreate.map(_.name) shouldBe List("pid1", "pid2", "employerid1", "employerid2", "name")
    employer.findIdIndex(main) shouldBe 2
    email.findIdIndex(main) shouldBe 0
    phone.findIdIndex(main) shouldBe 0
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

