/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.orm

import one.xingyi.core.UtilsSpec

class OrmSpec extends UtilsSpec with EntityFixture {

  behavior of "EntityStrategy"

  val es = EntityStrategy(m => m.tableName.tableName + "_m", o1 => o2 => o1.tableName.tableName + "/" + o2.tableName.tableName)
  val esSimple = EntityStrategy(m => m.tableName.tableName + "_s")
  val es2 = es.map(_ + "_done")

  it should "have a map method that produces a new strategy that is the composition of the two" in {
    es2.mainEntityFn(mainEntity) shouldBe "person_m_done"
    es2.childFn(mainEntity)(phoneEntity) shouldBe "person/phone_done"
  }

  it should "have a  child entity function that partially applies the parent " in {
    es.childFn(mainEntity)(phoneEntity) shouldBe "person/phone"
    es.childFn(phoneEntity)(phoneDetailsEntity) shouldBe "phone/phonedetails"
  }

  it should "have a walk method that walks the tree and returns a list of all places it visits and the result of the function" in {
    es.walk(mainEntity).map { case (e, x) => (e.tableName.tableName, x) } shouldBe List(
      ("person", "person_m"),
      ("address", "person/address"),
      ("phone", "person/phone"),
      ("phonedetails", "phone/phonedetails"))
  }

  it should "have a constructor that takes just one function and applies to both sides" in {
    esSimple.walk(mainEntity).map { case (e, x) => (e.tableName.tableName, x) } shouldBe List(
      ("person", "person_s"),
      ("address", "address_s"),
      ("phone", "phone_s"),
      ("phonedetails", "phonedetails_s"))
  }

  behavior of "OrmStrategies"

  it should "have a droptables strategy" in {
    val s = OrmStrategies.dropTables
    s.mainEntityFn(mainEntity) shouldBe "drop table if exists person"
    s.childFn(mainEntity)(phoneEntity) shouldBe "drop table if exists phone"
  }

  it should "have a createtables strategy" in {
    val s = OrmStrategies.createTables
    s.mainEntityFn(mainEntity) shouldBe "create table person (name varchar(255),id integer)"
    s.childFn(mainEntity)(phoneEntity) shouldBe "create table phone (manufacturer varchar(255),id integer,personId integer)"
  }

  it should "have a dropTempTables strategy" in {
    val s = OrmStrategies.dropTempTables
    s.mainEntityFn(mainEntity) shouldBe "drop table if exists temp_person"
    s.childFn(mainEntity)(phoneEntity) shouldBe "drop table if exists temp_phone"
  }

  it should "have a createTempTables strategy" in {
    val where = WhereForTableForTest("someWhere")

    val s = OrmStrategies.createTempTables(BatchDetails(123, 456, where))
    s.mainEntityFn(mainEntity) shouldBe "create temporary table temp_person as select p.name, p.id from person p where someWhere order by p.id limit 123 offset 56088"
    s.childFn(mainEntity)(phoneEntity) shouldBe "create temporary table temp_phone as select ph.manufacturer, ph.id, ph.personId from temp_person p,phone ph where p.id = ph.personId order by ph.personId,ph.id "
  }

  it should "have a drainTempTables strategy" in {
    val s = OrmStrategies.drainTempTables
    s.mainEntityFn(mainEntity) shouldBe "select * from temp_person"
    s.childFn(mainEntity)(phoneEntity) shouldBe "select * from temp_phone"
  }

  it should "have a insertData strategy" in {
    val s = OrmStrategies.insertData
    s.mainEntityFn(mainEntity) shouldBe "insert into person (name,id) values (?,?)"
    s.childFn(mainEntity)(phoneEntity) shouldBe "insert into phone (manufacturer,id,personId) values (?,?,?)"
  }


}
