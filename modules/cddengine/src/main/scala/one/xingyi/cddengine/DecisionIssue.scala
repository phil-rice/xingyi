package one.xingyi.cddengine
import one.xingyi.cddscenario.Scenario

abstract class DecisionIssue[P, R] (msg: String)extends RuntimeException(msg){
 def conclusionNode: ConclusionNode[P,R]
 def scenario: Scenario[P, R]
}

case class CannotAddScenarioBecauseClashes[P, R](scenario: Scenario[P, R], conclusionNode: ConclusionNode[P,R], clashesWith: List[Scenario[P, R]]) extends
  DecisionIssue[P,R](
    s"""Cannot add scenario\n${scenario.logic.definedInSourceCodeAt} $scenario
       |To conclusion node $conclusionNode
       |Because it clashes with
       |${clashesWith.map(s => s"${s.logic.definedInSourceCodeAt} $s").mkString("\n")}
       | """.stripMargin)