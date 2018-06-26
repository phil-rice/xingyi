package one.xingyi.cddexamples

import one.xingyi.cddengine.Engine
import one.xingyi.cddexamples.qAndA.MifidDecisionMaker
import one.xingyi.cddscalatest.CddFlatSpec

import scala.util.Try

class MifidSpec extends CddFlatSpec {
  override protected def engines: Try[List[Engine[_, _]]] = Try(List(new MifidDecisionMaker().categoriser))
}
