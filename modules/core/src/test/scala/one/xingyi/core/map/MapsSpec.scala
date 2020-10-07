/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.map

import one.xingyi.core.UtilsSpec
import Maps._
import one.xingyi.core.builder.HasId

class MapsSpec extends UtilsSpec {

  behavior of "Maps.addTo"

  it should "add to an empty map" in {
    Maps.addTo(Map("b" -> List(3)), "a", 1) shouldBe Map("a" -> List(1), "b" -> List(3))
  }
  it should "add to an existing key" in {
    Maps.addTo(Map("a" -> List(1), "b" -> List(3)), "a", 2) shouldBe Map("a" -> List(1, 2), "b" -> List(3))
  }

  behavior of "Maps.mergeAll"

  it should "Merge maps" in {
    Maps.mergeAll(Seq(Map("a" -> Seq(1), "b" -> Seq(2)), Map("a" -> Seq(3), "c" -> Seq(4)))) shouldBe
      Map("a" -> Seq(1, 3), "b" -> Seq(2), "c" -> Seq(4))
  }

  behavior of "MapsOps.optAdd"

  it should "add an item if 'some'" in {
    Map("a" -> 1).optAdd("b" -> Some(2)) shouldBe Map("a" -> 1, "b" -> 2)
  }
  it should "not add an item if 'none'" in {
    Map("a" -> 1).optAdd("b" -> None) shouldBe Map("a" -> 1)
  }

  behavior of "MapsOpt.add with a 'hasId' item"

  it should "add with the id" in {
    implicit val hasId: HasId[Int, String] = _ + "_id"
    Map("a" -> 1).add(2).add(3) shouldBe Map("a" -> 1, "2_id" -> 2, "3_id" -> 3)
  }

  behavior of "Maps.MapOfListsOps"

  it should "allow an item to be added to the list" in {
    Map("a" -> List(1)).addToList("a" -> 2).addToList("b" -> 3) shouldBe Map("a" -> List(1, 2), "b" -> List(3))
  }

  it should "return the list or nil for a key" in {
    Map("a" -> List(1)).items("a") shouldBe List(1)
    Map("a" -> List(1)).items("b") shouldBe List()
  }

  behavior of "ToMapOps"

  it should "allow a list of items to be turned into a map using a function" in {
    List(1, 2, 3).toMapFrom(_.toString) shouldBe Map("1" -> 1, "2" -> 2, "3" -> 3)
  }

  behavior of "ToJavaRecursively"

  it should "handle maps" in {
    val result = toJavaRecursively(Map("a" -> 1, "b" -> 2)).asInstanceOf[java.util.Map[String, Object]]
    result.size shouldBe 2
    result.get("a") shouldBe 1
    result.get("b") shouldBe 2
  }
  it should "handle lists" in {
    val result = toJavaRecursively(List("a", 1, "b", 2)).asInstanceOf[java.util.List[Object]]
    result.size shouldBe 4
    result.get(0) shouldBe "a"
    result.get(1) shouldBe 1
    result.get(2) shouldBe "b"
    result.get(3) shouldBe 2

  }
  it should "handle lists of maps" in {
    val (result: java.util.List[Object]) = toJavaRecursively(List(Map("a" -> 1, "b" -> 2))).asInstanceOf[java.util.List[Object]]
    result.size shouldBe 1
    val map = result.get(0).asInstanceOf[java.util.Map[String, Object]]
    map.size shouldBe 2
    map.get("a") shouldBe 1
    map.get("b") shouldBe 2
  }
  it should "handle maps of list" in {
    val result = toJavaRecursively(Map("a" -> List(1, 2), "b" -> 2)).asInstanceOf[java.util.Map[String, Object]]
    result.size shouldBe 2
    val list = result.get("a").asInstanceOf[java.util.List[Object]]
    list.size shouldBe 2
    list.get(0) shouldBe 1
    list.get(1) shouldBe 2
  }
}
