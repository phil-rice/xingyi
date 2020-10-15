/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.orm

import java.io.ByteArrayOutputStream
import java.sql.{PreparedStatement, ResultSet}
import java.util.concurrent.atomic.AtomicInteger

import javax.sql.DataSource
import one.xingyi.core.closable.ClosableM
import one.xingyi.core.jdbc.JdbcOps
import one.xingyi.core.json.JsonParser
import one.xingyi.core.orm.FieldType.{int, string}

import scala.language.{higherKinds, implicitConversions}

case class Employer(name: String)
case class Address(add: String)
case class Phone(phoneNo: String)
case class Person(name: String, employer: Employer, address: List[Address], phones: List[Phone], email: String)


trait OrmFixture extends SharedOrmFixture {
  implicit def fieldToKeys[T](f: FieldType[T]) = Keys(List(f))

  val employer = ManyToOneEntity(employerAlias, "E", int("eid"), int("employerid"), List(string("name")), List())
  val address = OneToManyEntity(addressAlias, "A", int("aid"), int("personid"), List(string("add")), List())
  val phone = OneToManyEntity(phoneAlias, "Ph", int("phid"), int("personid"), List(string("phoneNo")), List())
  //each person has a contact email, and the id of the email is the same as the person
  val email = SameIdEntity(emailAlias, "E", int("eid"), List(string("email")), List())
  val main = MainEntity(personAlias, "P", int("pid"), List(string("name")), List(employer, address, phone, email))


}

trait FastOrmFixture[M[_]] extends OrmFixture {
  implicit val maker: OrmMaker[Person] = { main =>
    data: Map[OrmEntity, List[List[Any]]] =>
      import OrmMaker._

      val eMap = employer.toMap(data, implicit list => Employer(str(0)))
      val aMap = address.toOneToManyMap(data, main, implicit list => Address(str(0)))
      val phoneMap = phone.toOneToManyMap(data, main, implicit list => Phone(str(0)))
      val emailMap = email.toMap(data, implicit list => str(0))
      data(main).map { implicit oneRow =>
        Person(str(0),
          employer.getData(main, eMap, oneRow),
          address.getData(main, aMap, oneRow),
          phone.getData(main, phoneMap, oneRow),
          email.getData(main, emailMap, oneRow))
      }.toStream
  }

  def setupPerson(ds: DataSource)(block: => Unit)(implicit jdbcOps: JdbcOps[DataSource], closableM: ClosableM[M]): Unit = {
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


abstract class AbstractFastOrmWithSingleLinkingKeysSpec[M[_] : ClosableM, J: JsonParser, DS <: DataSource] extends SharedFastOrmTests[M, J, DS] with FastOrmFixture[M] with CheckStrategyFixture {


  behavior of "FastOrm"

  it should "make drop table sql" in {
    checkStrategy("dropTable", OrmStrategies.dropTables.walk(main), List(
      main -> "drop table if exists Person",
      employer -> "drop table if exists Employer",
      address -> "drop table if exists Address",
      phone -> "drop table if exists Phone",
      email -> "drop table if exists ContactEmail"
    ))
  }
  it should "make dropTempTables sql" in {
    checkStrategy("dropTempTable", OrmStrategies.dropTempTables.walk(main), List(
      main -> "drop table if exists temp_Person",
      employer -> "drop table if exists temp_Employer",
      address -> "drop table if exists temp_Address",
      phone -> "drop table if exists temp_Phone",
      email -> "drop table if exists temp_ContactEmail"
    ))
  }

  it should "make drainTempTables sql" in {
    checkStrategy("drainTempTables", OrmStrategies.drainTempTables.walk(main), List(
      main -> "select * from temp_Person",
      employer -> "select * from temp_Employer",
      address -> "select * from temp_Address",
      phone -> "select * from temp_Phone",
      email -> "select * from temp_ContactEmail"
    ))
  }

  it should "make create table sql" in {
    checkStrategy("createTable", OrmStrategies.createTables.walk(main), List(
      main -> "create table Person (name varchar(255),employerid integer,pid integer)",
      employer -> "create table Employer (name varchar(255),eid integer)",
      address -> "create table Address (add varchar(255),aid integer,personid integer)",
      phone -> "create table Phone (phoneNo varchar(255),phid integer,personid integer)",
      email -> "create table ContactEmail (email varchar(255),eid integer)"
    ))
  }


  it should "make createTempTables sql" in {
    val where = WhereForTableForTest("someWhere")
    checkStrategy("createTempTables", OrmStrategies.createTempTables(BatchDetails(1000, 3, where)).walk(main), List(
      main -> "create temporary table temp_Person as select P.name, P.employerid, P.pid from Person P where someWhere order by P.pid limit 1000 offset 3000",
      employer -> "create temporary table temp_Employer as select E.name, E.eid from temp_Person P,Employer E where P.employerid = E.eid order by P.pid ",
      address -> "create temporary table temp_Address as select A.add, A.aid, A.personid from temp_Person P,Address A where P.pid = A.personid order by A.personid,A.aid ",
      phone -> "create temporary table temp_Phone as select Ph.phoneNo, Ph.phid, Ph.personid from temp_Person P,Phone Ph where P.pid = Ph.personid order by Ph.personid,Ph.phid ",
      email -> "create temporary table temp_ContactEmail as select DISTINCT  E.email, E.eid from temp_Person P,ContactEmail E where P.pid = E.eid order by E.eid "
    ))
  }

  it should "have a pretty print" in {
    checkStrings(main.prettyPrint(""),
      """MainEntity(Person, id=KeysAndIndex(2,pid), childrenAdded=employerid, data=name){
        |  ManyToOne(Employer, id=eid, idInParent=employerid data=name)
        |  OneToMany(Address, id=KeysAndIndex(1,aid), parent=personid data=add)
        |  OneToMany(Phone, id=KeysAndIndex(1,phid), parent=personid data=phoneNo)
        |  SameId(ContactEmail, id=eid, data=email)
        |}""".stripMargin)
  }

  it should "allow the turn the parent id into an fields with index " in {
    //documenting assumptions .. note this has been tested earlier 'in general' this is effectively an integration test
    main.fieldsAddedByChildren.map(_.name) shouldBe List("employerid")
    main.fieldsForCreate.map(_.name) shouldBe List("name", "employerid", "pid")

    employer.primaryKeyFieldsAndIndex shouldBe KeysAndIndex(List((1, FieldType("eid:int"))))
    phone.primaryKeyFieldsAndIndex shouldBe KeysAndIndex(List((1, FieldType("phid:int"))))
    address.primaryKeyFieldsAndIndex shouldBe KeysAndIndex(List((1, FieldType("aid:int"))))
    email.primaryKeyFieldsAndIndex shouldBe KeysAndIndex(List((1, FieldType("eid:int"))))
  }
  it should "have index and fields for the children" in {
    employer.idInParent.toKeysAndIndex(main) shouldBe KeysAndIndex(List((1, FieldType("employerid:int"))))
    phone.parentId.toKeysAndIndex(phone) shouldBe KeysAndIndex(List((2, FieldType("personid:int"))))
    address.parentId.toKeysAndIndex(address) shouldBe KeysAndIndex(List((2, FieldType("personid:int"))))
  }
}
