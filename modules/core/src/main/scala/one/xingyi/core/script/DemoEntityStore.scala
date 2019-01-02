package one.xingyi.core.script

import one.xingyi.core.builder.CopyWithNewId
import one.xingyi.core.json.ObjectProjection
import one.xingyi.core.language.AnyLanguage._
import one.xingyi.core.monad.{Monad, MonadCanFailWithException}

import scala.language.higherKinds

trait IEntityStore[M[_], T] {

  def getEntity: String => M[T]
  def putEntity: (String, T) => M[T]
  def newEntity: String => M[T]

}

trait IEntityStoreFailer[Fail] {
  def cannotFind(id: String): Fail
}

object IEntityStoreFailer {
  implicit object entityStoreFailerForThrowable extends IEntityStoreFailer[Throwable] {
    override def cannotFind(id: String): Throwable = new EntityStoreException(id)
  }
}

class EntityStoreException(msg: String) extends RuntimeException(msg)
object IEntityStore {
  def demo[M[_], Fail, SharedE, DomainE]
  (implicit monad: MonadCanFailWithException[M, Fail], failer: IEntityStoreFailer[Fail], changeId: CopyWithNewId[DomainE, String], objectProjection: ObjectProjection[SharedE, DomainE]): IEntityStore[M, DomainE] =
    new DemoEntityStore[M, Fail, SharedE, DomainE]
}

class DemoEntityStore[M[_], Fail, SharedE, DomainE](implicit monad: MonadCanFailWithException[M, Fail], failer: IEntityStoreFailer[Fail], changeId: CopyWithNewId[DomainE, String], objectProjection: ObjectProjection[SharedE, DomainE]) extends IEntityStore[M, DomainE] {
  var store = Map[String, DomainE]()
  newEntity apply "someName"

  def getEntity: String => M[DomainE] = id => store.get(id).fold[M[DomainE]](monad.fail(failer.cannotFind(id)))(_.liftM[M])
  def putEntity: (String, DomainE) => M[DomainE] = { (id, entity) => store = store + (id -> entity); entity.liftM }
  def newEntity = { id: String => putEntity(id, changeId(id, objectProjection.prototype)) }

}
