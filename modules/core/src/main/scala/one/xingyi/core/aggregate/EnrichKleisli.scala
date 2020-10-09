/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.aggregate
import one.xingyi.core.language.Language._
import one.xingyi.core.monad.Monad

import scala.language.higherKinds
import scala.reflect.ClassTag

trait HasChildren[Main, Children] extends (Main => Seq[Children])

trait HasChildrenForHolder[F[_]] {
  def apply[T](f: F[T]): List[F[_]]
  def descendants[T](f: F[T]): List[F[_]] = apply(f).flatMap(child => child :: descendants(child))
}
trait Enricher[Req, Parent, ChildId, Child, Res] extends ((Req, Parent, Seq[(ChildId, Child)]) => Res)

trait EnrichForTaglessLanguage[Wrapper[_, _]] {
  def enrichPrim[ReqP: ClassTag, ResP, ReqC, ResC, ResE: ClassTag](parent: Wrapper[ReqP, ResP], child: Wrapper[ReqC, ResC])(implicit findChildIds: HasChildren[ResP, ReqC], enricher: Enricher[ReqP, ResP, ReqC, ResC, ResE]): Wrapper[ReqP, ResE]

  case class enrich[ReqP: ClassTag, ResP](parent: Wrapper[ReqP, ResP]) {
    case class withChild[ReqC, ResC](child: Wrapper[ReqC, ResC])(implicit findChildIds: HasChildren[ResP, ReqC]) {
      def mergeInto[ResE: ClassTag](implicit enricher: Enricher[ReqP, ResP, ReqC, ResC, ResE]): Wrapper[ReqP, ResE] = enrichPrim(parent, child)
    }
  }

}
trait EnrichLanguage[M[_]] extends EnrichKleisli[M] {

  case class enrich[ReqP: ClassTag, ResP](parent: ReqP => M[ResP]) {
    case class withChild[ReqC, ResC](child: ReqC => M[ResC])(implicit findChildIds: HasChildren[ResP, ReqC]) {
      def mergeInto[ResE: ClassTag](implicit enricher: Enricher[ReqP, ResP, ReqC, ResC, ResE]): ReqP => M[ResE] = enrichPrim[ReqP, ResP, ReqC, ResC, ResE](parent, child)
    }
  }

}

trait EnrichKleisli[M[_]] {
  protected implicit def monad: Monad[M]
  private type Kleisli[Req, Res] = Req => M[Res]

  def enrichPrim[ReqP: ClassTag, ResP, ReqC, ResC, ResE: ClassTag](parentService: Kleisli[ReqP, ResP], childService: Kleisli[ReqC, ResC])(implicit findChildIds: HasChildren[ResP, ReqC], enricher: Enricher[ReqP, ResP, ReqC, ResC, ResE]): ReqP => M[ResE] =
    parentService |=++> { reqP => resP => findChildIds ~+> childService |=> (seq => enricher(reqP, resP, seq)) }

}
