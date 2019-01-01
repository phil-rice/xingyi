package one.xingyi.core.script

import one.xingyi.core.builder.CopyWithNewId
import one.xingyi.core.json.ObjectProjection
import one.xingyi.core.language.AnyLanguage._
import one.xingyi.core.monad.Monad

import scala.language.higherKinds

class EntityStore[M[_] : Monad, SharedE, DomainE](implicit changeId: CopyWithNewId[DomainE, String], objectProjection: ObjectProjection[SharedE, DomainE]) {

  var store = Map[String, DomainE]()
  newEntity apply "someName"

  def getEntity: String => M[DomainE] = id => store.getOrElse(id, throw new RuntimeException("could not find name: " + id)).liftM[M]
  def putEntity: (String, DomainE) => M[DomainE] = { (id, entity) => store = store + (id -> entity); entity.liftM }
  def newEntity = { id: String => putEntity(id, changeId(id, objectProjection.prototype)) }

}
