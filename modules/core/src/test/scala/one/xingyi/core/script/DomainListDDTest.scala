package one.xingyi.core.script
import one.xingyi.core.UtilsSpec
import one.xingyi.core.json.{JsonWriter, ToJsonLib}

abstract class DomainListDDTest[J](implicit jsonWriter: JsonWriter[J]) extends UtilsSpec with ScriptFixture {

  behavior of "DomainListDD to Json"

  it should "turn a domainAndMethodList into Json" in {
    val domainListDD: DomainListDD = implicitly[DomainAndMethodListToDisplayDom] apply listOfDomainAndMethods1
    jsonWriter(implicitly[ToJsonLib[DomainListDD]] apply domainListDD).noWhiteSpace shouldBe
      """{
        |  "selected":{
        |    "name":"ParentDomainForTest1",
        |    "methods":[{
        |      "url":"/parent/<id>",
        |      "verb":"Get"
        |    },{
        |      "url":"/parent/<id>",
        |      "verb":"Post"
        |    }],
        |    "entities":{
        |      "one.xingyi.core.script.IChild":{
        |
        |      },
        |      "one.xingyi.core.script.IHouse":{
        |
        |      },
        |      "one.xingyi.core.script.IParent":{
        |        "nameLens":"lens_parent_name_string",
        |        "houseLens":"lens_parent_house_house"
        |      }
        |    },
        |    "renderers":["renderer1","renderer2"]
        |  },
        |  "domains":[{
        |    "name":"ParentDomainForTest1",
        |    "methods":[{
        |      "url":"/parent/<id>",
        |      "verb":"Get"
        |    },{
        |      "url":"/parent/<id>",
        |      "verb":"Post"
        |    }],
        |    "entities":{
        |      "one.xingyi.core.script.IChild":{
        |
        |      },
        |      "one.xingyi.core.script.IHouse":{
        |
        |      },
        |      "one.xingyi.core.script.IParent":{
        |        "nameLens":"lens_parent_name_string",
        |        "houseLens":"lens_parent_house_house"
        |      }
        |    },
        |    "renderers":["renderer1","renderer2"]
        |  },{
        |    "name":"ParentDomainForTest2",
        |    "methods":[{
        |      "url":"/parent/<id>",
        |      "verb":"Get"
        |    },{
        |      "url":"/parent/<id>",
        |      "verb":"Post"
        |    }],
        |    "entities":{
        |      "one.xingyi.core.script.IChild":{
        |
        |      },
        |      "one.xingyi.core.script.IHouse":{
        |
        |      },
        |      "one.xingyi.core.script.IParent":{
        |        "nameLens":"lens_parent_name_string",
        |        "childrenLens":"lens_parent_children_childlist"
        |      }
        |    },
        |    "renderers":["renderer1","renderer2"]
        |  }]
        |}""".stripMargin.noWhiteSpace
  }
}
