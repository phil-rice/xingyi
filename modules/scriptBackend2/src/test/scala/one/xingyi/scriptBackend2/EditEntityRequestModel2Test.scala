package one.xingyi.scriptBackend2

import one.xingyi.core.script.EditEntityRequestTest
import one.xingyi.json4s.Json4sParser._
import one.xingyi.json4s.Json4sWriter._
import one.xingyi.scriptModel2.IPerson
import org.json4s.JsonAST.JValue


class EditEntityRequestModel2Test extends EditEntityRequestTest[JValue, IPerson, Person] {
  override def dom: Person = Person("someName", Address("someLine1", "someLine2", "pc"), Telephone("someTelNo"))
}
