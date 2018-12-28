package one.xingyi.scriptExample.createdCode1


import one.xingyi.scriptModel1._
import one.xingyi.core.optics.Lens
import one.xingyi.core.script.{Domain,DomainMaker, IXingYi,ServerDomain}


case class Person (mirror: Object) extends Domain with IPerson
object Person {
  implicit object default extends DomainMaker[Person] {
    override def create(mirror: Object): Person = Person(mirror)
  }
}
case class TelephoneNumber (mirror: Object) extends Domain with ITelephoneNumber
object TelephoneNumber {
  implicit object default extends DomainMaker[TelephoneNumber] {
    override def create(mirror: Object): TelephoneNumber = TelephoneNumber(mirror)
  }
}


class PersonNameOps(implicit val xingYi: IXingYi) extends IPersonNameOps[Lens, Person]{
   def nameLens = xingYi.stringLens[Person]("lens_person_name_string")
}
class PersonTelephoneOps(implicit val xingYi: IXingYi) extends IPersonTelephoneOps[Lens, Person,TelephoneNumber]{
   def telephoneNumberLens = xingYi.objectLens[Person,TelephoneNumber]("lens_person_telephonenumber_telephonenumber")
}
class PersonLine12Ops(implicit val xingYi: IXingYi) extends IPersonLine12Ops[Lens, Person]{
   def line1Lens = xingYi.stringLens[Person]("lens_person_line1_string")
   def line2Lens = xingYi.stringLens[Person]("lens_person_line2_string")
}
class TelephoneNumberOps(implicit val xingYi: IXingYi) extends ITelephoneNumberOps[Lens, TelephoneNumber]{
   def numberLens = xingYi.stringLens[TelephoneNumber]("lens_telephonenumber_number_string")
}





object Model1Domain extends ServerDomain{
   def lens=List("lens_telephonenumber_number_string", "lens_person_line2_string", "lens_person_line1_string", "lens_person_telephonenumber_telephonenumber", "lens_person_name_string")
}