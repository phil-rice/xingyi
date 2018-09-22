package one.xingyi.core.orm
import one.xingyi.core.UtilsSpec

class OrmSpec extends UtilsSpec with EntityFixture {

  behavior of "EntityStrategy"

  val es = EntityStrategy(m => m.tableName + "_m", o1 => o2 => o1.tableName + "/" + o2.tableName)
  val esSimple = EntityStrategy(m => m.tableName + "_s")
  val es2 = es.map(_ + "_done")

  it should "have a map method that produces a new streategy that is the composition of the two" in {
    es2.mainEntityFn(mainEntity) shouldBe "person_m_done"
    es2.oneToManyEntityFn(mainEntity)(phoneEntity) shouldBe "person/phone_done"
  }

  it should "have a  child entity function that partially applies the parent " in {
    es.childEntity(mainEntity)(phoneEntity) shouldBe "person/phone"
    es.childEntity(phoneEntity)(phoneDetailsEntity) shouldBe "phone/phonedetails"
  }

  it should "have a walk method that walks the tree and returns a list of all places it visits and the result of the function" in {
    es.walk(mainEntity).map { case (e, x) => (e.tableName, x) } shouldBe List(
      ("person", "person_m"),
      ("address", "person/address"),
      ("phone", "person/phone"),
      ("phonedetails", "phone/phonedetails"))
  }

  it should "have a constructor that takes just one function and applies to both sides" in {
    esSimple.walk(mainEntity).map { case (e, x) => (e.tableName, x) } shouldBe List(
      ("person", "person_s"),
      ("address", "address_s"),
      ("phone", "phone_s"),
      ("phonedetails", "phonedetails_s"))
  }

  behavior of "OrmStrategies"

  it should "have a droptables strategy" in {
    val s = OrmStrategies.dropTables
    s.mainEntityFn(mainEntity) shouldBe "drop table if exists person"
    s.oneToManyEntityFn(mainEntity)(phoneEntity) shouldBe "drop table if exists phone"
  }

  it should "have a createtables strategy" in {
    val s = OrmStrategies.createTables
    s.mainEntityFn(mainEntity) shouldBe "create table person (id integer,name varchar(255))"
    s.oneToManyEntityFn(mainEntity)(phoneEntity) shouldBe "create table phone (id integer,personId integer,manufacturer varchar(255))"
  }

  it should "have a dropTempTables strategy" in {
    val s = OrmStrategies.dropTempTables
    s.mainEntityFn(mainEntity) shouldBe "drop table if exists temp_person"
    s.oneToManyEntityFn(mainEntity)(phoneEntity) shouldBe "drop table if exists temp_phone"
  }

  it should "have a createTempTables strategy" in {
    val s = OrmStrategies.createTempTables(BatchDetails(123, 456))
    s.mainEntityFn(mainEntity) shouldBe "create temporary table temp_person as select p.id, p.name from person p limit 123 offset 56088"
    s.oneToManyEntityFn(mainEntity)(phoneEntity) shouldBe "create temporary table temp_phone as select ph.personId, ph.id, ph.manufacturer from temp_person p,phone ph where p.id = ph.personId"
  }

  it should "have a drainTempTables strategy" in {
    val s = OrmStrategies.drainTempTables
    s.mainEntityFn(mainEntity) shouldBe "select * from temp_person"
    s.oneToManyEntityFn(mainEntity)(phoneEntity) shouldBe "select * from temp_phone"
  }

  it should "have a insertData strategy" in {
    val s = OrmStrategies.insertData
    s.mainEntityFn(mainEntity) shouldBe "insert into person (id,name) values (?,?)"
    s.oneToManyEntityFn(mainEntity)(phoneEntity) shouldBe "insert into phone (id,personId,manufacturer) values (?,?,?)"
  }


}
