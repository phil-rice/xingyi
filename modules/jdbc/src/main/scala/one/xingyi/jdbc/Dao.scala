package one.xingyi.jdbc

import scala.language.higherKinds
import one.xingyi.core.language.MonadLanguage._
import one.xingyi.core.closable.ClosableLanguage._
import one.xingyi.core.monad.{Liftable, Monad, MonadWithException}
import one.xingyi.core.language.AnyLanguage._
case class Id(id: String) extends AnyVal
trait DaoWriter[M[_]] extends (DaoMap => M[Id])
trait DaoReader[M[_]] extends (Id => M[Option[DaoMap]])

trait DaoWriterOps[M[_], T] {
  def save: T => M[Id]
}

trait DaoReaderOps[M[_], T] {
  def get: Id => M[Option[T]]
}
object SimpleDaoOps {
  def simpleWriterDaoOps[M[_], T](implicit toDaoMap: ToDaoMap[T], daoWriter: DaoWriter[M]): DaoWriterOps[M, T] = new DaoWriterOps[M, T] {
    def save = toDaoMap andThen daoWriter
  }
  def simpleReaderDaoOps[M[_] : Monad, T](implicit fromDaoMap: FromDaoMap[T], daoReader: DaoReader[M]): DaoReaderOps[M, T] = new DaoReaderOps[M, T] {
    def get = daoReader |?> fromDaoMap
  }
}

case class DigestFieldName[T](name: FieldDefn) extends AnyVal
case class DigestSalts(salt: String, oldSalts: List[String])
trait DigestString extends (DaoMap => String)

trait DigestMap[T] extends (DaoMap => String)
object DigestMap {
  implicit def defaultDigestMap[T](implicit schema: Schema[T], digestSalts: DigestSalts): DigestMap[T] = daoMap =>
    (digestSalts.salt :: schema.fields.filter(_.digestable).map(d => d.typeName.digest(daoMap(d)))).mkString(",")
}

trait AddDigestToDaoMap[T] extends (DaoMap => DaoMap)
object AddDigestToDaoMap {
  implicit def defaultDaoMapOps[T](implicit digestMap: DigestMap[T], digestFieldName: DigestFieldName[T]): AddDigestToDaoMap[T] = new AddDigestToDaoMap[T] {
    def apply(daoMap: DaoMap) = daoMap + (digestFieldName.name -> digestMap(daoMap))
  }
}
case class DigestMismatchException(expectedDigest: Any, actualDigest: String) extends RuntimeException
trait CheckDigest[M[_], T] extends (DaoMap => M[DaoMap])
object CheckDigest {
  implicit def defaultCheckDigest[M[_] : MonadWithException, T](implicit digestMap: DigestMap[T], digestFieldName: DigestFieldName[T]): CheckDigest[M, T] = { daoMap =>
    if (digestMap(daoMap) == daoMap(digestFieldName.name)) daoMap.liftM else new DigestMismatchException(daoMap(digestFieldName.name), digestMap(daoMap)).liftException
  }
}
object DigesterDaoOps {
  implicit def writerDaoOps[M[_], T](implicit addDigest: AddDigestToDaoMap[T], toDaoMap: ToDaoMap[T], daoWriter: DaoWriter[M]): DaoWriterOps[M, T] = new DaoWriterOps[M, T] {
    def save: T => M[Id] = toDaoMap andThen addDigest andThen daoWriter
  }
  implicit def readerDaoOps[M[_] : Monad, T](implicit checkDigest: CheckDigest[M, T], fromDaoMap: FromDaoMap[T], daoReader: DaoReader[M]): DaoReaderOps[M, T] = new DaoReaderOps[M, T] {
    def get: Id => M[Option[T]] = daoReader |??> checkDigest |?> fromDaoMap
  }
}


//trait DatabaseEntity[Main <: DatabaseEntity[Main, ID], ID] extends PrettyPrintable with AuditPrintable with Deletable[Main] {
//  def ivDetails: Option[IVDetails[ID]]
//  def id = ivDetails.get.identityDetails.id
//  def optId = ivDetails.map(_.identityDetails.id)
//  def version = ivDetails.get.valueDetails.version
//  def withIvDetails(ivDetails: Option[IVDetails[ID]]): Main
//  def dataJson: String
//}
//
//trait TableDetails[ID] extends PrettyPrintable with AuditPrintable {
//  def id: ID
//  def version: Long
//  def auditJson: String
//  def auditPrint = prettyPrint("")
//  def signature: String
//
//}
