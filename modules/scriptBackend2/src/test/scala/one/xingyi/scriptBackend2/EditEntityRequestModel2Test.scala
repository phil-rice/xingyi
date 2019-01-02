package one.xingyi.scriptBackend2

import one.xingyi.core.script.{AbstractEntityServiceFinderRequestTest, EditEntityRequestTest, EntityRequestTest}
import one.xingyi.json4s.Json4sParser._
import one.xingyi.json4s.Json4sWriter._
import one.xingyi.scriptModel2.IPerson
import org.json4s.JsonAST.JValue


class EditEntityRequestModel2Test extends EditEntityRequestTest[JValue, IPerson, Person] {
  override def dom: Person = Person(id1, Address("someLine1", "someLine2", "pc"), Telephone("someTelNo"))
}

class EntityRequestModel2Test extends EntityRequestTest[JValue, IPerson, Person]

class EntityServiceFinderRequestModel2Test extends AbstractEntityServiceFinderRequestTest[JValue]