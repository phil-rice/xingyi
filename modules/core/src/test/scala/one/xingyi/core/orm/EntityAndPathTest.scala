package one.xingyi.core.orm

import one.xingyi.core.UtilsSpec
import one.xingyi.core.strings.Strings

class EntityAndPathTest extends UtilsSpec with OrmKeySpecFixture {

  val t1a: SchemaForTest = "t1/a"

  val s1: List[SchemaForTest] = List(SchemaItemWithChildren("main", false, List[SchemaForTest](
    "t1/a", "t2/b", "t1/c", "t2/d",
    SchemaItemWithChildren("child1", true, List[SchemaForTest](
      "t3/x", "t3/y"
    )))))

  behavior of "Entity And Path"

  def checkEntityAndPath(entityAndPaths: TablesAndFieldsAndPaths)(expected: String) = {
    val actual = entityAndPaths.prettyPrint.mkString("\n")
    checkStrings(actual, expected)
  }

  it should "turn a schema into a list of tables and names" in {
    val keys: OrmKeys[SchemaForTest] = OrmKeys(s1)
    checkNumericKeys(keys)(
      """main (),0 OneChild
        | t1/a (0),0 NoChildren
        | t2/b (0),1 NoChildren
        | t1/c (0),2 NoChildren
        | t2/d (0),3 NoChildren
        | child1 (0),4 ManyChildren
        |  t3/x (0,4),0 NoChildren
        |  t3/y (0,4),1 NoChildren""".stripMargin)
    val entityAndPaths: TablesAndFieldsAndPaths = EntityAndPath(keys)
    println(s"Entity and paths: $entityAndPaths")
    //    println(entityAndPaths.prettyPrint.mkString("\n"))
    checkEntityAndPath(entityAndPaths)(
      """|t1
         |   0 a - (0) - 0
         |   1 c - (0) - 2
         |t2
         |   0 b - (0) - 1
         |   1 d - (0) - 3
         |t3
         |   0 x - (0,4) - 0
         |   1 y - (0,4) - 1""".stripMargin)
  }

  behavior of "OrmFactory"

  lazy val ormFactory = {
    val keys: OrmKeys[SchemaForTest] = OrmKeys(s1)
    val entityAndPaths: TablesAndFieldsAndPaths = EntityAndPath(keys)
    entityAndPaths.ormFactory(keys)
  }
  it should "create an orm with just the main" in {
    val mainEntity = ormFactory.mainEntity(TableName("t1", ""), "one", Keys("t1Id"), List())
    checkStrings(mainEntity.entity.prettyPrint(""), """MainEntity(t1, id=KeysAndIndex(2,t1Id), data=a,c)""")
    mainEntity.entity.fieldsForCreate.size shouldBe 3
  }

}
