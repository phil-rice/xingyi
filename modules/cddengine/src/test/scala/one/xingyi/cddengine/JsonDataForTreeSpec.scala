package one.xingyi.cddengine
import one.xingyi.core.UtilsSpec
import one.xingyi.core.json.{JsonObject, JsonWriter}

class JsonDataForTreeSpec [J:JsonWriter] extends UtilsSpec{

  behavior of "JsonDataForTree"

  it should "have a make method that creates a Json Data, putting the 'ScenarioAndPathThroughTree' into a Some()" in {
    val jsonObject = mock[JsonObject]
    val data = mock[ScenarioAndPathThroughTree[String, String]]
    val jsonData = JsonDataForTree.make(data)(jsonObject)
    jsonData.data shouldBe Some(data)
    jsonData.jsonObject shouldBe jsonObject
  }

}
