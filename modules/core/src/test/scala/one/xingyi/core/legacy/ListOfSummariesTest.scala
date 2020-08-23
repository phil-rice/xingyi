package one.xingyi.core.legacy

import one.xingyi.core.MonoidSpec

class ListOfSummariesTest extends MonoidSpec[ListOfSummaries] {
  val summaryOfDifferenceNoDiff = SummaryOfDifference(ID("id0"), List())
  val summaryOfDifference1 = SummaryOfDifference(ID("id1"), List("1-1", "1-2"))
  val summaryOfDifference2 = SummaryOfDifference(ID("id2"), List("2-1", "2-2"))
  //Note that this is identical to ListofSummaries(List())... b
  override def zeroValue: ListOfSummaries = ListOfSummaries(summaryOfDifferenceNoDiff)
  override def one: ListOfSummaries = ListOfSummaries(summaryOfDifference1)
  override def two: ListOfSummaries = ListOfSummaries(summaryOfDifference2)
  override def three: ListOfSummaries = ListOfSummaries(List(summaryOfDifference1, summaryOfDifference2))

}
