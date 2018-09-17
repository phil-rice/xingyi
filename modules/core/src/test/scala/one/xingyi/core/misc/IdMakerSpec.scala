package one.xingyi.core.misc
import one.xingyi.core.UtilsSpec

class IdMakerSpec extends UtilsSpec with PublicIdMaker{

  "idMaker/publicIdMaker" should "return the next id" in {
    getNextId shouldBe 0
    getNextId shouldBe 1
    getNextId shouldBe 2
  }
}
