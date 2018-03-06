package org.validoc.utils.aggregate

import org.validoc.utils.functions.Monad
import org.validoc.utils.language.Language._

import scala.language.higherKinds

trait HasChildren[Main, Children] extends (Main => Seq[Children])

trait Enricher[Req, Parent, ChildId, Child, Res] extends ((Req, Parent, Seq[(ChildId, Child)]) => Res)

trait EnrichLanguage[Wrapper[_, _]] {
  def enrichPrim[ReqP, ResP, ReqC, ResC, ResE](parent: Wrapper[ReqP, ResP], child: Wrapper[ReqC, ResC])(implicit findChildIds: HasChildren[ResP, ReqC], enricher: Enricher[ReqP, ResP, ReqC, ResC, ResE]): Wrapper[ReqP, ResE]

  case class enrich[ReqP, ResP](parent: Wrapper[ReqP, ResP]) {
    case class withChild[ReqC, ResC](child: Wrapper[ReqC, ResC])(implicit findChildIds: HasChildren[ResP, ReqC]) {
      def mergeInto[ResE](implicit enricher: Enricher[ReqP, ResP, ReqC, ResC, ResE]): Wrapper[ReqP, ResE] = enrichPrim(parent, child)
    }
  }

}

trait EnrichKleisli[M[_]] {
  protected implicit def monad: Monad[M]
  private type Kleisli[Req, Res] = Req => M[Res]

  def enrichPrim[ReqP, ResP, ReqC, ResC, ResE](parentService: Kleisli[ReqP, ResP], childService: Kleisli[ReqC, ResC])(implicit findChildIds: HasChildren[ResP, ReqC], enricher: Enricher[ReqP, ResP, ReqC, ResC, ResE]): ReqP => M[ResE] =
    parentService |=++> { reqP => resP => findChildIds ~+> childService |=> (seq => enricher(reqP, resP, seq)) }

}
