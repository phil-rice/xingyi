package one.xingyi.core.orm

import one.xingyi.core.UtilsSpec
import one.xingyi.core.jdbc.FastOrmFixture
import one.xingyi.core.strings.ParseException

trait OrmDslClass {
  protected def wrap[X](msg: String)(block: => X): X = try {block } catch {case e: Exception => throw new ParseException(s" $msg - ${e.getClass.getSimpleName} ${e.getMessage}", e)}

}
trait ValidateAndBuild[OrmEntity] extends ((List[FieldType], List[ChildEntity]) => OrmEntity)

case class FieldsWord[OrmEntity](nameAndTypes: Seq[String])(implicit validateAndBuild: ValidateAndBuild[OrmEntity]) extends OrmDslClass {
  val dataFieldTypes = wrap("dataFieldTypes")(nameAndTypes.map(FieldType.parse).toList)
  def children(childEntities: ChildEntity*): OrmEntity = validateAndBuild(dataFieldTypes, childEntities.toList)
  def noChildren: OrmEntity = validateAndBuild(dataFieldTypes, List())
}

abstract class ormDslTable[OrmEntity](tableName: String, primaryKeyDefn: String, aliasOverride: String = "") extends OrmDslClass {
  protected def validate(childEntity: List[ChildEntity]): Unit = childEntity.foreach {
    case o: OneToManyEntity => if (o.parentId.getClass != primaryKey.getClass) throw new RuntimeException(s"One to many table ${o.tableName} has parentId ${o.parentId} which is incompatible with parent ${tableName} which has primary key ${primaryKey}")
    case _ =>
  }
  implicit val validateAndBuild: ValidateAndBuild[OrmEntity] = (dataFields: List[FieldType], children: List[ChildEntity]) => {
    validate(children)
    build(dataFields, children)
  }
  def fields(nameAndTypes: String*): FieldsWord[OrmEntity] = FieldsWord(nameAndTypes)
  protected def build(dataFields: List[FieldType], children: List[ChildEntity]): OrmEntity
  require(tableName.nonEmpty)
  val primaryKey: FieldType = wrap(s"$tableName / primarykey")(FieldType.parse(primaryKeyDefn))
  val alias: String = if (aliasOverride == "") tableName.take(1) else aliasOverride
}


case class orm(tableName: String, primaryKeyDefn: String, aliasOverride: String = "") extends ormDslTable[MainEntity](tableName, primaryKeyDefn, aliasOverride) {
  override protected def build(dataFields: List[FieldType], children: List[ChildEntity]) = {
    MainEntity(tableName, alias, primaryKey, dataFields, children)
  }
}

case class oneToMany(tableName: String, primaryKeyDefn: String, parentIdDefn: String, aliasOverride: String = "") extends ormDslTable[OneToManyEntity](tableName, primaryKeyDefn, aliasOverride) {
  val parentId = FieldType.parse(parentIdDefn)
  override protected def build(dataFields: List[FieldType], children: List[ChildEntity]): OneToManyEntity =
    OneToManyEntity(tableName, alias, primaryKey, parentId, dataFields, children);
}
case class manyToOne(tableName: String, primaryKeyDefn: String, idInParentDefn: String, aliasOverride: String = "") extends ormDslTable[ManyToOneEntity](tableName, primaryKeyDefn, aliasOverride) {
  val idInParent = FieldType.parse(idInParentDefn)
  override protected def build(dataFields: List[FieldType], children: List[ChildEntity]): ManyToOneEntity =
    ManyToOneEntity(tableName, alias, primaryKey, idInParent, dataFields, children);
}

class OrmDslTest extends UtilsSpec with FastOrmFixture {

  behavior of "OrmDsl"

  it should "create the person entity using the DSL" in {
    //val employer = ManyToOneEntity("Employer", "E", IntField("eid"), IntField("employerid"), List(StringField("name")), List())
    val personViaDsl = orm("Person", "pid:int").fields("name: string").children(
      manyToOne("Employer", "eid:int", "employerid:int").fields("name:string").noChildren,
      oneToMany("Address", "aid: int", "personid:int").fields("add:string").noChildren,
      oneToMany("Phone", "aid: int", "personid:int", aliasOverride = "Ph").fields("phoneNo:string").noChildren)

    personViaDsl shouldBe main
  }

  it should "default table alias to the first letter of the table name, but use override if given" in {
    orm("Person", "pid:int").fields("name: string").noChildren.alias shouldBe "P"
    orm("Person", "pid:int", aliasOverride = "Pe").fields("name: string").noChildren.alias shouldBe "Pe"
  }

  it should "throw an exception if a oneToMany child has the wrong parentId type" in {
    the[RuntimeException] thrownBy orm("Person", "pid:int").fields("name: string").children(
      oneToMany("Address", "aid: int", "personid:string").fields("add:string").noChildren) should have message ("One to many table Address has parentId StringField(personid,varchar(255)) which is incompatible with parent Person which has primary key IntField(pid,integer)")
  }

}
