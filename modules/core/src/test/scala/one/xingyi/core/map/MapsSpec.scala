package one.xingyi.core.map
import one.xingyi.core.UtilsSpec

class MapsSpec extends UtilsSpec {

  behavior of "Maps.addTo"

  it should "add to an empty map" in {
    Maps.addTo(Map("b" -> List(3)), "a", 1) shouldBe Map("a" -> List(1), "b" -> List(3))
  }
  it should "add to an existing key" in {
    Maps.addTo(Map("a" -> List(1), "b" -> List(3)), "a", 2) shouldBe Map("a" -> List(1, 2), "b" -> List(3))
  }


}
