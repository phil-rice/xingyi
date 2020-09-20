package one.xingyi.core.orm

import one.xingyi.core.UtilsSpec
import one.xingyi.core.closable.SimpleClosable

class OrmDslTest extends UtilsSpec with FastOrmFixture[SimpleClosable] {

  behavior of "OrmDsl"

  it should "create the person entity using the DSL" in {
    val personViaDsl = orm(personTable, "pid:int").fields("name: string").children(
      manyToOne(employerTable, "eid:int").idInParent("employerid:int").fields("name:string").noChildren,
      oneToMany(addressTable, "aid: int").parentId("personid:int").fields("add:string").noChildren,
      oneToMany(phoneTable, "phid: int", aliasOverride = "Ph").parentId("personid:int").fields("phoneNo:string").noChildren,
      sameId(emailTable, "eid: int", aliasOverride = "E").fields("email:string").noChildren)
    personViaDsl shouldBe main
  }

  it should "default table alias to the first letter of the table name, but use override if given" in {
    orm(personTable, "pid:int").fields("name: string").noChildren.alias shouldBe "P"
    orm(personTable, "pid:int", aliasOverride = "Pe").fields("name: string").noChildren.alias shouldBe "Pe"
  }

  it should "throw an exception if a oneToMany child has the wrong parentId type" in {
    the[RuntimeException] thrownBy
      orm(personTable, "pid:int").fields("name: string").children(
        oneToMany(addressTable, "aid: int").parentId("personid:string").fields("add:string").noChildren) should have
    message("One to many table Address has parentId StringField(personid,varchar(255)) which is incompatible with parent Person which has primary key IntField(pid,integer)")
  }

}
