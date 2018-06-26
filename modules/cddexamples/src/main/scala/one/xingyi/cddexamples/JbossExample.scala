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
