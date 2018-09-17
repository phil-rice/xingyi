/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.jdbc


import one.xingyi.core.language.FunctionLanguage._
import one.xingyi.core.language.MonadFunctionLanguage._
import one.xingyi.core.monad.Monad

import scala.language.higherKinds
trait DaoInserter[M[_]] {
  def insert[ID, T](implicit schema: Schema[ID, T]): DaoMap => M[ID]
}
trait DaoUpdater[M[_]] {
  def update[ID, T](implicit schema: Schema[ID, T]): DaoMap => M[ID]
}

case class WithHistory[T](main: T, history: Seq[T])
trait DaoReader[M[_]] {
  def read[ID, T](implicit schema: Schema[ID, T]): ID => M[Option[DaoMap]]
  def readWithHistory[ID, T](implicit schema: Schema[ID, T]): ID => M[Option[WithHistory[DaoMap]]]
}

class Dao[M[_] : Monad, T](implicit schema: Schema[Int, T], addDigest: AddDigestToDaoMap[T], toDaoMap: ToDaoMap[T], checkDigest: CheckDigest[M, T], daoWriter: DaoInserter[M], daoReader: DaoReader[M], fromDaoMap: FromDaoMap[T]) {
  type EntityId = Int
  type ChildID = Int
  val pointToChildField = FieldDefn("child", IntFieldType, true)
  def addField[V](fieldDefn: FieldDefn): V => DaoMap => DaoMap = { value => _ + (fieldDefn -> value) }
  def addFirstChildField: DaoMap => DaoMap = addField(pointToChildField)(0)
  def addChildField: EntityId => DaoMap => DaoMap = addField(pointToChildField)

  def insertChild: EntityId => T => M[ChildID] = { id: EntityId => toDaoMap.child ~> addDigest ~> daoWriter.insert }

  def insertMain: T => M[EntityId] = updateMain(0)
  def updateMain: EntityId => T => M[EntityId] = { id => toDaoMap.main ~> addChildField(id) ~> addDigest ~> daoWriter.insert }


  def insert: T => M[ChildID] = insertMain |==+/> insertChild
  def update: EntityId => T => M[ChildID] = insertChild
  def find: EntityId => M[Option[WithHistory[T]]] = daoReader.readWithHistory |??> checkDigest |?> fromDaoMap.fromDaoHistory
  def transform(fn: T => T): EntityId => M[Option[ChildID]] = find |?> (_.main) |?> fn |+??> updateMain
}


case class DigestFieldName[T](name: FieldDefn) extends AnyVal
case class DigestSalts(salt: String, oldSalts: List[String])
trait DigestString extends (DaoMap => String)

trait DigestMap[T] extends (DaoMap => String)

object DigestMap {
  implicit def defaultDigestMap[ID, T](implicit schema: Schema[ID, T], digestSalts: DigestSalts): DigestMap[T] = daoMap =>
    (digestSalts.salt :: schema.fieldsToDigest.filter(_.digestable).map(d => d.typeName.digest(daoMap(d)))).mkString(",")
}

trait AddDigestToDaoMap[T] extends (DaoMap => DaoMap)
object AddDigestToDaoMap {
  implicit def defaultDaoMapOps[T](implicit digestMap: DigestMap[T], digestFieldName: DigestFieldName[T]): AddDigestToDaoMap[T] = new AddDigestToDaoMap[T] {
    def apply(daoMap: DaoMap) = daoMap + (digestFieldName.name -> digestMap(daoMap))
  }
}
case class DigestMismatchException(expectedDigest: Any, actualDigest: String) extends RuntimeException
trait CheckDigest[M[_], T] extends (WithHistory[DaoMap] => M[WithHistory[DaoMap]])
object CheckDigest {
  //  implicit def defaultCheckDigest[M[_] : MonadWithException, T](implicit digestMap: DigestMap[T], digestFieldName: DigestFieldName[T]): CheckDigest[M, T] = { history =>
  //  TODO need to deal
  //  with old salt values
  //    if (digestMap(history) == history(digestFieldName.name)) history.liftM else new DigestMismatchException(history(digestFieldName.name), digestMap(history)).liftException
  //  }
}
