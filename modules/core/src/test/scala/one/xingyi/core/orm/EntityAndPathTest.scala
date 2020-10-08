package one.xingyi.core.orm

import one.xingyi.core.UtilsSpec
import one.xingyi.core.strings.Strings

class EntityAndPathTest extends UtilsSpec with OrmKeySpecFixture {

  val t1a: SchemaForTest[String] = "t1/a"

  val s1: List[SchemaForTest[_]] = List(SchemaItemWithChildren("main", false, List[SchemaForTest[_]](
    "t1/a", "t2/b", "t1/c", "t2/d",
    SchemaItemWithChildren("child1", true, List[SchemaForTest[_]](
      "t3/x", "t3/y"
    )))))

  behavior of "Entity And Path"

  def checkEntityAndPath(entityAndPaths: TablesAndFieldsAndPaths)(expected: String) = {
    val actual = entityAndPaths.prettyPrint.mkString("\n")
    checkStrings(actual, expected)
  }


  it should "turn a schema into a list of tables and names" in {
    val keys: OrmKeys[SchemaForTest] = OrmKeys.fromList(s1)
    checkKeys(keys)(
      """main:O(),0 OneChild
        | t1/c:L(0),0 NoChildren
        | child1:O(0),1 ManyChildren
        |  t3/x:S(0,1),0 NoChildren
        |  t3/y:S(0,1),1 NoChildren
        | t1/a:S(0),2 NoChildren
        | t2/b:S(0),3 NoChildren
        | t2/d:S(0),4 NoChildren""".stripMargin)
    val entityAndPaths: TablesAndFieldsAndPaths = EntityAndPath(keys)
    println(s"Entity and paths: $entityAndPaths")
    //    println(entityAndPaths.prettyPrint.mkString("\n"))
    checkEntityAndPath(entityAndPaths)(
      """t1
        |   0 c/varchar(255) - (0) - 0
        |   1 a/varchar(255) - (0) - 2
        |t2
        |   0 b/varchar(255) - (0) - 3
        |   1 d/varchar(255) - (0) - 4
        |t3
        |   0 x/varchar(255) - (0,1) - 0
        |   1 y/varchar(255) - (0,1) - 1""".stripMargin)
  }

  behavior of "OrmFactory"

  lazy val ormFactory = {
    val keys: OrmKeys[SchemaForTest] = OrmKeys.fromList(s1)
    val entityAndPaths: TablesAndFieldsAndPaths = EntityAndPath(keys)
    entityAndPaths.ormFactory(keys)
  }
  it should "create an orm with just the main" in {
    val mainEntity = ormFactory.mainEntity(TableName("t1", ""), "one", Keys("t1Id"), List())
    checkStrings(mainEntity.entity.prettyPrint(""), """MainEntity(t1, id=KeysAndIndex(2,t1Id), data=c,a)""")
    mainEntity.entity.fieldsForCreate.size shouldBe 3
  }

}
