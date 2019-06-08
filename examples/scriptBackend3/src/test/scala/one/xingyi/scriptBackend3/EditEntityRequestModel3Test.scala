package one.xingyi.scriptBackend3

import one.xingyi.core.script.{AbstractEntityServiceFinderRequestTest, EditEntityRequestTest, EntityRequestTest}
import one.xingyi.json4s.Json4sParser._
import one.xingyi.json4s.Json4sWriter._
import one.xingyi.scriptModel3.IPerson
import org.json4s.JsonAST.JValue

class EditEntityRequestModel3Test extends EditEntityRequestTest[JValue, IPerson, Person] {
  override def dom: Person = Person(id1, List(Address("someLine1", "someLine2", "pc")), Telephone("someTelNo"))
}

class EntityRequestModel3Test extends EntityRequestTest[JValue, IPerson, Person]

class EntityServiceFinderRequestModel3Test extends AbstractEntityServiceFinderRequestTest[JValue]