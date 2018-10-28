package one.xingyi.cddengine
import one.xingyi.cddscenario.Scenario
import one.xingyi.core.UtilsSpec

class DecisionTreeFoldingDataSpec extends UtilsSpec with DecisionTreeFixture {

  behavior of "DecisionTreeFoldingData"

  case class DecisionIssueForTest[P, R](conclusionNode: ConclusionNode[P, R], scenario: Scenario[P, R]) extends DecisionIssue[P, R]("someIssue")

  val issue1 = DecisionIssueForTest[String, String](conca, snormal)
  val issue2 = DecisionIssueForTest[String, String](concb, sa)

//  lazy val data = DecisionTreeFoldingData(treeNormalPassport, mock[])

  //  it should "turn an issue into a DecisionTree, like the old one but with one extra issue" in {
  //    val treeWithIssue1 = treeNormalPassport.copy(issues = List(issue1))
  //    val treeWithIssue1And2 = treeNormalPassport.copy(issues = List(issue1, issue2))
  //    DecisionTreeFolder.recordError(treeNormalPassport)(issue1) shouldBe treeWithIssue1
  //    DecisionTreeFolder.recordError(treeWithIssue1)(issue2) shouldBe treeWithIssue1And2
  //  }
  //  it should "throw exceptions that aren't decision issues" in {
  //    val exception = new RuntimeException
  //    intercept[RuntimeException](DecisionTreeFolder.recordError(mock[DecisionTree[String, String]])(exception)) shouldBe exception
  //
  //  }
}
