package one.xingyi.cddexamples

import one.xingyi.cddengine.{CddRunner, Engine}
import one.xingyi.cddscalatest.CddFlatSpec

import scala.util.Try

class JBossExampleSpec extends CddFlatSpec {
  override protected def engines: Try[List[Engine[_, _]]] = Try(List(new JbossExample().categorise))
}
