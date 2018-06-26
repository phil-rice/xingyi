package one.xingyi.cddengine
import one.xingyi.core.UtilsSpec
import one.xingyi.core.json.{JsonList, JsonObject, JsonWriterLangauge}

trait RenderingFixture extends EngineFixture with JsonWriterLangauge {
  implicit val url = new SimpleUrlGenerator[String, String]()
  val s = new SimpleDecisionTreeRendering[String, String]()

  val uc1s0 = usecase1.allScenarios(0)
  val uc1s1 = usecase1.allScenarios(1)
  val uc2s0 = usecase2.allScenarios(0)
  val uc2s1 = usecase2.allScenarios(1)
  val jsa = JsonObject("situation" -> "a", "url" -> url.scenario(uc1s0), "defined" -> uc1s0.definedInSourceCodeAt.toString)
  val jsaw = JsonObject("situation" -> "aw", "url" -> url.scenario(uc1s1), "defined" -> uc1s1.definedInSourceCodeAt.toString)
  val jsb = JsonObject("situation" -> "b", "url" -> url.scenario(uc2s0), "defined" -> uc2s0.definedInSourceCodeAt.toString)
  val jsbw = JsonObject("situation" -> "bw", "url" -> url.scenario(uc2s1), "defined" -> uc2s1.definedInSourceCodeAt.toString)
  val juc1 = JsonObject("useCase" -> JsonObject("scenarioTitle" -> true,
    "scenarios" -> JsonList(List(jsa, jsaw)),
    "url" -> url.usecase(usecase1),
    "defined" -> usecase1.definedInSourceCodeAt.toString,
    "title" -> "use case about A"))

  val juc2 = JsonObject("useCase" -> JsonObject("scenarioTitle" -> true,
    "scenarios" -> JsonList(List(jsb, jsbw)),
    "url" -> url.usecase(usecase2),
    "defined" -> usecase2.definedInSourceCodeAt.toString,
    "title" -> "use case about B"))

  val jconcNodetrue = JsonObject("conclusionNode" -> JsonObject("scenarios" -> JsonList(List(jsa, jsaw))), "defined" -> uc1s0.definedInSourceCodeAt.toString)
  val jconcNodeFalse = JsonObject("conclusionNode" -> JsonObject("scenarios" -> JsonList(List(jsb, jsbw))), "defined" -> uc2s0.definedInSourceCodeAt.toString)
  val jDecNodeBody = JsonObject("condition" -> "EngineFixture.this.containsA", "ifTrue" -> jconcNodetrue, "ifFalse" -> jconcNodeFalse)
  val jDecisionNode = JsonObject("decisionNode" -> jDecNodeBody, "defined" -> uc1s0.definedInSourceCodeAt.toString)
  val jTree = JsonObject("root" -> JsonObject("decisionNode" -> jDecNodeBody, "defined" -> usecase1.allScenarios(0).definedInSourceCodeAt.toString))
  val jEngine = JsonObject("useCases" -> JsonList(List(juc1, juc2)), "tree" -> jTree, "url" -> url.engine(e))

}
class SimpleDecisionTreeRenderingSpec[J] extends UtilsSpec with RenderingFixture {

  behavior of "SimpleDecisionTreeRendering"

  it should "turn an engine into a Json object " in {
    s.engine(e) shouldBe jEngine


  }
  it should "turn an tree into a Json object " in {
    s.tree(e.tools.decisionTree) shouldBe jTree

  }
  it should "turn an usecase into a Json object " in {
    s.useCase(usecase1) shouldBe juc1
    s.useCase(usecase2) shouldBe juc2
  }
  it should "turn an scenario into a Json object " in {
    s.scenario(uc1s0) shouldBe jsa
    s.scenario(uc1s1) shouldBe jsaw
  }
  it should "turn an conclusionnode into a Json object when it knows it is a conclusion node " in {
    s.conclusionNode(e.tools.decisionTree.root.asInstanceOf[DecisionNode[String, String]].ifTrue.asInstanceOf[ConclusionNode[String, String]]) shouldBe jconcNodetrue
    s.conclusionNode(e.tools.decisionTree.root.asInstanceOf[DecisionNode[String, String]].ifFalse.asInstanceOf[ConclusionNode[String, String]]) shouldBe jconcNodeFalse
  }
  it should "turn an conclusionnode into a Json object when using node " in {
    s.node(e.tools.decisionTree.root.asInstanceOf[DecisionNode[String, String]].ifTrue) shouldBe jconcNodetrue
    s.node(e.tools.decisionTree.root.asInstanceOf[DecisionNode[String, String]].ifFalse) shouldBe jconcNodeFalse

  }
  it should "turn an decisionNode into a Json object when it knows it is a decisionNode node " in {
    s.decisionNode(e.tools.decisionTree.root.asInstanceOf[DecisionNode[String, String]]) shouldBe jDecisionNode
  }
  it should "turn an decisionNode into a Json object when using node " in {
    s.node(e.tools.decisionTree.root) shouldBe jDecisionNode
  }
  it should "turn an issues into a Json object when using node " ignore {
    fail
  }
  it should "implement andThen" in {
    val f = { j: JsonObject => j.toString }
    val t = (s andThen f).asInstanceOf[TransformingTreeRending[JsonObject, String, String, String]]
    t.transform shouldBe f
    t.rendering shouldBe s
  }
}


class TransformingTreeRendingSpec extends UtilsSpec with RenderingFixture {
  behavior of "TransformingTreeRending"

  val t = s andThen (_.toString)

  it should "turn an engine into a string " in {
    t.engine(e) shouldBe jEngine.toString
  }

  it should "turn an tree into a string " in {
    t.tree(e.tools.decisionTree) shouldBe jTree.toString

  }
  it should "turn an usecase into a string " in {
    t.useCase(usecase1) shouldBe juc1.toString
    t.useCase(usecase2) shouldBe juc2.toString
  }
  it should "turn an scenario into a string " in {
    t.scenario(uc1s0) shouldBe jsa.toString
    t.scenario(uc1s1) shouldBe jsaw.toString
  }
  it should "turn an conclusionnode into a string when it knows it is a conclusion node " in {
    t.conclusionNode(e.tools.decisionTree.root.asInstanceOf[DecisionNode[String, String]].ifTrue.asInstanceOf[ConclusionNode[String, String]]) shouldBe jconcNodetrue.toString
    t.conclusionNode(e.tools.decisionTree.root.asInstanceOf[DecisionNode[String, String]].ifFalse.asInstanceOf[ConclusionNode[String, String]]) shouldBe jconcNodeFalse.toString
  }
  it should "turn an conclusionnode into a string when using node " in {
    t.node(e.tools.decisionTree.root.asInstanceOf[DecisionNode[String, String]].ifTrue) shouldBe jconcNodetrue.toString
    t.node(e.tools.decisionTree.root.asInstanceOf[DecisionNode[String, String]].ifFalse) shouldBe jconcNodeFalse.toString

  }
  it should "turn an decisionNode into a string when it knows it is a decisionNode node " in {
    t.decisionNode(e.tools.decisionTree.root.asInstanceOf[DecisionNode[String, String]]) shouldBe jDecisionNode.toString
  }
  it should "turn an decisionNode into a string when using node " in {
    t.node(e.tools.decisionTree.root) shouldBe jDecisionNode.toString
  }
  it should "turn an issues into a Json object when using node " ignore {
    fail
  }
  it should "implement andThen" in {
    val f = { s: String => s + s }
    val t2 = (t andThen f).asInstanceOf[TransformingTreeRending[JsonObject, String, String, String]]
    t2.transform shouldBe f
    t2.rendering shouldBe t
  }
}