/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
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
