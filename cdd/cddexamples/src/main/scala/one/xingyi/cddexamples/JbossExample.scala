/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddexamples

import one.xingyi.cddengine.{Engine, UseCase1}


case class Person(savings: Int, ageInYears: Int) {
  lazy val hasEnoughSavings = savings >= 1000
  lazy val tooYoung = ageInYears < 16
}
class JbossExample {
  type PersonUC = UseCase1[Person, String]
  val categorise = Engine(new PersonUC("This is the example from JBoss") {
    scenario(Person(savings = 1050, ageInYears = 20)) produces "person.valid"
    scenario(Person(savings = 10000, ageInYears = 10)) produces "person.invalid.child" when (_.ageInYears < 16)
    scenario(Person(savings = 50, ageInYears = 20)) produces "person.invalid.tooPoor" when (_.savings < 1000)
  })
}
