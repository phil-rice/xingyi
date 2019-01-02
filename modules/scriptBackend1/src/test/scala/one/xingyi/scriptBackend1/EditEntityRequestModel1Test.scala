package one.xingyi.scriptBackend1
import one.xingyi.core.script.EditEntityRequestTest
import one.xingyi.scriptModel1.IPerson
import org.json4s.JsonAST.JValue
import one.xingyi.json4s.Json4sParser._
import one.xingyi.json4s.Json4sWriter._

class EditEntityRequestModel1Test extends EditEntityRequestTest[JValue, IPerson, Person] {
  override def dom: Person = Person(id1, "someLine1", "someLine2", Telephone("someTelNo"))
}
