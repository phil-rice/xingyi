package one.xingyi.core.map

import one.xingyi.core.UtilsSpec

import java.util.concurrent.atomic.AtomicInteger

class SingletonMapTest extends UtilsSpec {

  behavior of "SingletonMap"

  it should "return the same value for the same key" in {
    val count = new AtomicInteger()
    val map = new SingletonMap[String, String](s => s + count.getAndIncrement())
    map("first") shouldBe "first0"
    map("first") shouldBe "first0"
    map("first") shouldBe "first0"
    map("second") shouldBe "second1"
    map("second") shouldBe "second1"
    map("first") shouldBe "first0"
  }

}
