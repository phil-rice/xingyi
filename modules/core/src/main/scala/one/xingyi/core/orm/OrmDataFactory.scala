package one.xingyi.core.orm

trait OrmDataFactory[MainT, T] {
  def apply[Context](e: MainT, contextMaker: () => Context, data: Map[T, Array[List[Any]]], fn: (Context, T, List[Any]) => Context): MainOrmData[Context, MainT]
}


class OrmDataFactoryForMainEntity extends OrmDataFactory[MainEntity, OrmEntity] {
  def childOrmData[Context](p: OrmEntity, data: Map[OrmEntity, Array[List[Any]]], fn: (Context, OrmEntity, List[Any]) => Context)(e: ChildEntity): OrmData[Context] = {
    def children(c: ChildEntity): List[OrmData[Context]] = c.children.map(childOrmData(e, data, fn))
    e match {
      case o: OneToManyEntity => FanoutOrmData(o, o.tableName.tableName, FlyweightKey(p.primaryKeyFieldsAndIndex, o.parentIdsAndIndex), fn, data(o), children(o))
      case o: OneToZeroOneEntity => FanoutOrmData(o, o.tableName.tableName, FlyweightKey(p.primaryKeyFieldsAndIndex, o.primaryKeyFieldsAndIndex), fn, data(o), children(o))
      case s: SameIdEntity => FanoutOrmData(s, s.tableName.tableName, FlyweightKey(p.primaryKeyFieldsAndIndex, s.primaryKeyFieldsAndIndex), fn, data(s), children(s))
      case m: ManyToOneEntity => FanInOrmData(m, m.tableName.tableName, idInParentData = m.idInParent.toKeysAndIndex(p).getKey, idForChild = m.primaryKeyFieldsAndIndex.getKey, fn, data(m), children(m))
    }
  }


  override def apply[Context](e: MainEntity, contextMaker: () => Context, data: Map[OrmEntity, Array[List[Any]]], fn: (Context, OrmEntity, List[Any]) => Context): MainOrmData[Context, MainEntity] =
    MainOrmData(e, e.tableName.tableName, contextMaker, fn, data(e), e.children.map(childOrmData(e, data, fn)))

}
