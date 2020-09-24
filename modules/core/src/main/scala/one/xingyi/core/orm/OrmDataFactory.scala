package one.xingyi.core.orm

trait OrmDataFactory[MainT, T] {
  def apply(e: MainT, data: Map[T, Array[List[Any]]], fn: (T, List[Any]) => Unit): MainOrmData[MainT]
}


class OrmDataFactoryForMainEntity extends OrmDataFactory[MainEntity, OrmEntity] {
  def childOrmData(p: OrmEntity, data: Map[OrmEntity, Array[List[Any]]], fn: (OrmEntity, List[Any]) => Unit)(e: ChildEntity): OrmData = {
    def children(c: ChildEntity): List[OrmData] = c.children.map(childOrmData(e, data, fn))
    e match {
      case o: OneToManyEntity => FanoutOrmData(o, FlyweightKey(p.primaryKeyFieldsAndIndex, e.primaryKeyFieldsAndIndex), fn, data(o), children(o))
      case o: OneToZeroOneEntity => FanoutOrmData(o, FlyweightKey(p.primaryKeyFieldsAndIndex, o.primaryKeyFieldsAndIndex), fn, data(o), children(o))
      case m: ManyToOneEntity => FanInOrmData(m, idInParentData = m.idInParent.toKeysAndIndex(p).getKey, idForChild = m.primaryKeyFieldsAndIndex.getKey, fn, data(m), children(m))
      case s: SameIdEntity => FanoutOrmData(s, FlyweightKey(p.primaryKeyFieldsAndIndex, s.primaryKeyFieldsAndIndex), fn, data(s), children(s))
    }
  }

  override def apply(e: MainEntity, data: Map[OrmEntity, Array[List[Any]]], fn: (OrmEntity, List[Any]) => Unit): MainOrmData[MainEntity] =
    MainOrmData(e, fn, data(e), e.children.map(childOrmData(e, data, fn)))

}
