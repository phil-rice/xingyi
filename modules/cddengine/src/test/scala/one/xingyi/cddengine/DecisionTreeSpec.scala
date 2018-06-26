/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddengine
import one.xingyi.core.UtilsSpec

class DecisionTreeSpec extends UtilsSpec with DecisionTreeFixture {

  behavior  of "DecisionTree"

  it should "allow parents to be found from situation" in {
    DecisionTree.findWithParents(treeNormalGun, "gun") shouldBe List(dNormalGun,concGun )
    DecisionTree.findWithParents(treeNormalGun, "password") shouldBe List(dNormalGun,concNormal )
    DecisionTree.findWithParents(treeNormalPassport, "gun") shouldBe List(dNormalPassport,concNoPassport)
  }
  it should "allow parents to be found from scenario" in {
    DecisionTree.findWithParentsForScenario(treeNormalGun)(sgun) shouldBe List(dNormalGun,concGun )
    DecisionTree.findWithParentsForScenario(treeNormalGun)(snormal) shouldBe List(dNormalGun,concNormal )
    DecisionTree.findWithParentsForScenario(treeNormalPassport)(sgunNoPassport) shouldBe List(dNormalPassport,concNoPassport)
  }
}
