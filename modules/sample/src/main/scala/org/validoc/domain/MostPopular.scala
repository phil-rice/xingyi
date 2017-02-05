package org.validoc.domain


import org.validoc.utils.aggregate.{Enricher, HasChildren}

case class MostPopular(programmeIds: Seq[ProgrammeId])

object MostPopular {

  implicit object HasChildrenForMostPopular extends HasChildren[MostPopular, ProgrammeId] {
    override def apply(p: MostPopular): Seq[ProgrammeId] = p.programmeIds
  }

}

case class EnrichedMostPopular(programmes: Seq[Programme])

object EnrichedMostPopular {

  implicit object EnricherForMostPopular extends Enricher[EnrichedMostPopular, MostPopular, Programme] {
    override def apply(p: MostPopular)(children: Seq[Programme]): EnrichedMostPopular =
      EnrichedMostPopular( children)
  }
  def apply(p: MostPopular, children: Seq[Programme]): EnrichedMostPopular =
    EnrichedMostPopular(children)
}