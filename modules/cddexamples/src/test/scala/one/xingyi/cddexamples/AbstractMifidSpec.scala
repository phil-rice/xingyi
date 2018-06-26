package one.xingyi.cddexamples

import java.util.ResourceBundle

import one.xingyi.cddengine.Engine
import one.xingyi.cddexamples.qAndA.MifidDecisionMaker
import one.xingyi.cddscalatest.CddFlatSpec
import one.xingyi.core.optics.AbstractMifidBlackboardSpec
import org.json4s.JValue
import one.xingyi.json4s.Json4sWriter._

import scala.util.Try

class MifidSpec extends CddFlatSpec {
  override protected def engines: Try[List[Engine[_, _]]] = Try(List(new MifidDecisionMaker().categoriser))
}

class MifidBlackboardSpec extends AbstractMifidBlackboardSpec[JValue]{
  implicit val bundle = ResourceBundle.getBundle("message")
}
