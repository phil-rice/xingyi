package one.xingyi.core.orm

import one.xingyi.core.UtilsSpec
import one.xingyi.core.jdbc.FastOrmFixture
import one.xingyi.core.strings.ParseException

trait OrmDslClass {
  protected def wrap[X](msg: String)(block: => X): X = try {block } catch {case e: Exception => throw new ParseException(s" $msg - ${e.getClass.getSimpleName} ${e.getMessage}", e)}
}
trait BuildOrmEntity[OrmEntity] extends ((List[FieldType], List[ChildEntity]) => OrmEntity)

case class FieldsWord[OrmEntity](nameAndTypes: Seq[String])(implicit validateAndBuild: BuildOrmEntity[OrmEntity]) extends OrmDslClass {
  val dataFieldTypes = wrap("dataFieldTypes")(nameAndTypes.map(FieldType.parse).toList)
  def children(childEntities: ChildEntity*): OrmEntity = validateAndBuild(dataFieldTypes, childEntities.toList)
  def noChildren: OrmEntity = validateAndBuild(dataFieldTypes, List())
}

abstract class ormDslTable[OrmEntity](tableName: String, primaryKeyDefn: String, aliasOverride: String = "") extends OrmDslClass {
  require(tableName.nonEmpty)
  protected val primaryKey: FieldType = wrap(s"$tableName / primarykey")(FieldType.parse(primaryKeyDefn))
  protected val alias: String = if (aliasOverride == "") tableName.take(1) else aliasOverride

  protected def validate(childEntity: List[ChildEntity]): Unit = childEntity.foreach {
    case o: OneToManyEntity => if (o.parentId.getClass != primaryKey.getClass) throw new RuntimeException(s"One to many table ${o.tableName} has parentId ${o.parentId} which is incompatible with parent ${tableName} which has primary key ${primaryKey}")
    case _ =>
  }
}


case class orm(tableName: String, primaryKeyDefn: String, aliasOverride: String = "") extends ormDslTable[MainEntity](tableName, primaryKeyDefn, aliasOverride) {
  private implicit val mainValidateAndBuild: BuildOrmEntity[MainEntity] = { (dataFields, children) =>
    validate(children);
    MainEntity(tableName, alias, primaryKey, dataFields, children)
  }
  def fields(nameAndTypes: String*): FieldsWord[MainEntity] = FieldsWord(nameAndTypes)
}
case class oneToMany(tableName: String, primaryKeyDefn: String, aliasOverride: String = "") extends ormDslTable[OneToManyEntity](tableName, primaryKeyDefn, aliasOverride) {
  case class parentId(parentIdDefn: String) {
    private val parentId = FieldType.parse(parentIdDefn)
    implicit val oneToManyValidateAndBuild: BuildOrmEntity[OneToManyEntity] = { (dataFields, children) =>
      validate(children);
      OneToManyEntity(tableName, alias, primaryKey, parentId, dataFields, children)
    }
    def fields(nameAndTypes: String*): FieldsWord[OneToManyEntity] = FieldsWord(nameAndTypes)
  }
}
case class manyToOne(tableName: String, primaryKeyDefn: String, aliasOverride: String = "") extends ormDslTable[ManyToOneEntity](tableName, primaryKeyDefn, aliasOverride) {
  case class idInParent(idInParentDefn: String) {
    private val idInParent = FieldType.parse(idInParentDefn)
    private implicit val ManyToOneValidateAndBuild: BuildOrmEntity[ManyToOneEntity] = { (dataFields, children) =>
      validate(children);
      ManyToOneEntity(tableName, alias, primaryKey, idInParent, dataFields, children)
    }
    def fields(nameAndTypes: String*): FieldsWord[ManyToOneEntity] = FieldsWord(nameAndTypes)
  }
}

class OrmDslTest extends UtilsSpec with FastOrmFixture {

  behavior of "OrmDsl"

  it should "create the person entity using the DSL" in {
    val personViaDsl = orm("Person", "pid:int").fields("name: string").children(
      manyToOne("Employer", "eid:int").idInParent("employerid:int").fields("name:string").noChildren,
      oneToMany("Address", "aid: int").parentId("personid:int").fields("add:string").noChildren,
      oneToMany("Phone", "aid: int", aliasOverride = "Ph").parentId("personid:int").fields("phoneNo:string").noChildren)

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
