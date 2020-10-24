package one.xingyi.core.accessors

import scala.language.higherKinds

trait HasNameF[F[_]] {def apply[T](f: F[T]): String}
trait HasDescriptionF[F[_]] {def apply[T](f: F[T]): String}
trait HasFullNameF[F[_]] {def apply[T](t: F[T]): String}
trait HasParentsF[F[_]] {def apply[T](t: F[T]): List[F[_]]}

trait HasChildrenF[F[_]] {
  def apply[T](f: F[T]): List[F[_]]
  def descendants[T](f: F[T]): List[F[_]] = apply(f).flatMap(child => child :: descendants(child))
}

object HigherOrderAccessorsLanguage extends HigherOrderAccessorsLanguage
trait HigherOrderAccessorsLanguage {
  implicit class HasNameOps[F[_], T](f: F[T])(implicit hasName: HasNameF[F]) {def name: String = hasName(f)}
  implicit class HasDescriptionOps[F[_], T](f: F[T])(implicit hasDescription: HasDescriptionF[F]) {def description: String = hasDescription(f)}
  implicit class HasFullNameOps[F[_], T](f: F[T])(implicit hasFullName: HasFullNameF[F]) {def fullName: String = hasFullName(f)}
  implicit class HasParentOps[F[_], T](f: F[T])(implicit hasParents: HasParentsF[F]) {def parents: List[F[_]] = hasParents(f)}
  implicit class HasChildrenOps[F[_], T](f: F[T])(implicit hasChildren: HasChildrenF[F]) {
    def children: List[F[_]] = hasChildren(f)
    def descendants: List[F[_]] = hasChildren.descendants(f)
  }

}