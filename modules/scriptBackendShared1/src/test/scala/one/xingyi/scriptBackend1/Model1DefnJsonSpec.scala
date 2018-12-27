/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.scriptBackend1

import one.xingyi.core.UtilsSpec
import one.xingyi.core.json.{JsonParser, JsonWriter, ObjectProjection}
import one.xingyi.scriptModel1.IPerson
import one.xingyi.scriptSharedBackend.PersonStore

abstract class Model1DefnJsonSpec[J: JsonParser](implicit writer: JsonWriter[J]) extends UtilsSpec with PersonStore[Person] {

  behavior of "Example Domain ToJson using projections"

  val json =
    """{
  "name":"someName",
  "telephoneNumber":{
    "number":"someNumber"
  },
  "addresses":[{
    "line1":"line1",
    "line2":"line2",
    "postcode":"pc1"
  },{
    "line1":"line2",
    "line2":"line2",
    "postcode":"pc2"
  }]
}"""
  it should "use the projection to turn a person into json" in {
    writer(implicitly[ObjectProjection[IPerson, Person]].toJson(person)).noWhiteSpace shouldBe
      json.stripMargin.noWhiteSpace
  }

  it should "use the projection to turn json into a person" in {
    implicitly[ObjectProjection[IPerson, Person]].fromJson[J](json) shouldBe person
  }
}
