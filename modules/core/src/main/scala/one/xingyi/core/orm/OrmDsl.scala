package one.xingyi.core.orm


import one.xingyi.core.strings.ParseException

trait OrmDslClass {
  protected def wrap[X](msg: String)(block: => X): X = try {block } catch {case e: Exception => throw new ParseException(s" $msg - ${e.getClass.getSimpleName} ${e.getMessage}", e)}
}
trait BuildOrmEntity[OrmEntity] extends ((List[FieldType[_]], List[ChildEntity]) => OrmEntity)

case class FieldsWord[OrmEntity](nameAndTypes: Seq[String])(implicit validateAndBuild: BuildOrmEntity[OrmEntity]) extends OrmDslClass {
  val dataFieldTypes = wrap("dataFieldTypes")(nameAndTypes.map(FieldType.parse).toList)
  def children(childEntities: ChildEntity*): OrmEntity = validateAndBuild(dataFieldTypes, childEntities.toList)
  def noChildren: OrmEntity = validateAndBuild(dataFieldTypes, List())
}

abstract class ormDslTable[OrmEntity](tableName: String, primaryKeyDefn: String, aliasOverride: String = "") extends OrmDslClass {
  require(tableName.nonEmpty)
  protected val primaryKey: FieldType[_] = wrap(s"$tableName / primarykey")(FieldType.parse(primaryKeyDefn))
  protected val alias: String = if (aliasOverride == "") tableName.take(1) else aliasOverride

  protected def validate(childEntity: List[ChildEntity]): Unit = childEntity.foreach {
    case o: OneToManyEntity => if (o.parentId.classTag.runtimeClass != primaryKey.classTag.runtimeClass) throw new RuntimeException(s"One to many table ${o.tableName} has parentId ${o.parentId} which is incompatible with parent ${tableName} which has primary key ${primaryKey}")
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
case class sameId(tableName: String, primaryKeyDefn: String, aliasOverride: String = "") extends ormDslTable[SameIdEntity](tableName, primaryKeyDefn, aliasOverride) {
  def fields(nameAndTypes: String*): FieldsWord[SameIdEntity] = {
    implicit val ManyToOneValidateAndBuild: BuildOrmEntity[SameIdEntity] = { (dataFields, children) =>
      validate(children);
      SameIdEntity(tableName, alias, primaryKey, dataFields, children)
    }
    FieldsWord(nameAndTypes)
  }
}
