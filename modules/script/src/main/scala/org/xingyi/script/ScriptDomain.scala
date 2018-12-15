package org.xingyi.script

trait ScriptDomain {
  def renderers: List[String]
  def lens: Seq[LensDefn[_,_]]
  def name = getClass.getSimpleName
}


object ScriptDomain