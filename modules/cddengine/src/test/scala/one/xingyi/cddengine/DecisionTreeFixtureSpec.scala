/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddengine
import one.xingyi.cddscenario._
import one.xingyi.core.reflection.DefinedInSourceCodeAt


trait DecisionTreeLanguage[P, R] {
  val cEmpty = ConclusionNode[P, R](List(), ScenarioLogic.empty)
  def s(p: P, r: R) = Scenario[P, R](p, Some(r), ScenarioLogic.empty, List(), EngineComponentData(DefinedInSourceCodeAt.definedInSourceCodeAt(), Some(s"$p => $r")))
  def s(p: P, r: R, when: P => Boolean) = Scenario[P, R](p, Some(r), WhenResultScenarioLogic[P, R](when, r, DefinedInSourceCodeAt.definedInSourceCodeAt(), ""), List(), EngineComponentData(DefinedInSourceCodeAt.definedInSourceCodeAt(), Some(s"$p => $r")))

  def c(s: Scenario[P, R], ss: Scenario[P, R]*) = ConclusionNode(s :: ss.toList, s.logic)
  def c(logic: ScenarioLogic[P, R], ss: Scenario[P, R]*) = ConclusionNode(ss.toList, logic)
  def d(right: Scenario[P, R], left: Scenario[P, R]) = DecisionNode(right.logic, c(left), c(right))
  def d(logic: ScenarioLogic[P, R], left: DecisionTreeNode[P, R], right: DecisionTreeNode[P, R]) = DecisionNode(logic, left, right)
  def d(logic: Scenario[P, R], left: DecisionTreeNode[P, R], right: DecisionTreeNode[P, R]) = DecisionNode(logic.logic, left, right)
  def t(dn: DecisionTreeNode[P, R]) = DecisionTree(dn, List())

}

trait DecisionTreeFixture extends ScenarioFixture with DecisionTreeLanguage[String, String] {


  val concNormal = c(snormal)
  val concNormal1And2 = c(snormal, snormal2)
  val concNoPassport = c(sNoPassport)
  val concGunNoPassport = c(sgunNoPassport)
  val concGun = c(sgun)
  val concGunGunNoPassport = c(sgun, sgunNoPassport)
  val dNormalPassport = d(sNoPassport.logic, concNormal, concNoPassport)
  val dNormalGun = d(sgun.logic, concNormal, concGun)
  val dGunNoPassword = d(sNoPassport.logic, concGun, concNoPassport)

  val treeNormalPassport = DecisionTree(dNormalPassport, List())
  val treeNormalGun = DecisionTree(dNormalGun, List())

  val conca = c(sa)
  val concawa = c(sawa)
  val concb = c(sb)
  val concbwb = c(sbwb)
}
