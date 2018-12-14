package org.xingyi.script

trait ScriptDomain {
  def renderers: List[String]
  def name = getClass.getSimpleName
}
