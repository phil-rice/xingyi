package one.xingyi.core.orm

import one.xingyi.core.UtilsSpec

class OrmDslTest extends UtilsSpec with FastOrmFixture {

  behavior of "OrmDsl"

  it should "create the person entity using the DSL" in {
    val personViaDsl = orm("Person", "pid:int").fields("name: string").children(
      manyToOne("Employer", "eid:int").idInParent("employerid:int").fields("name:string").noChildren,
      oneToMany("Address", "aid: int").parentId("personid:int").fields("add:string").noChildren,
      oneToMany("Phone", "phid: int", aliasOverride = "Ph").parentId("personid:int").fields("phoneNo:string").noChildren,
      sameId("ContactEmail", "eid: int", aliasOverride = "E").fields("email:string").noChildren)
    personViaDsl shouldBe main
  }

  it should "default table alias to the first letter of the table name, but use override if given" in {
    orm("Person", "pid:int").fields("name: string").noChildren.alias shouldBe "P"
    orm("Person", "pid:int", aliasOverride = "Pe").fields("name: string").noChildren.alias shouldBe "Pe"
  }

  it should "throw an exception if a oneToMany child has the wrong parentId type" in {
    the[RuntimeException] thrownBy
      orm("Person", "pid:int").fields("name: string").children(
        oneToMany("Address", "aid: int").parentId("personid:string").fields("add:string").noChildren) should have
    message("One to many table Address has parentId StringField(personid,varchar(255)) which is incompatible with parent Person which has primary key IntField(pid,integer)")
  }

}
