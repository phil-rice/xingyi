/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
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
