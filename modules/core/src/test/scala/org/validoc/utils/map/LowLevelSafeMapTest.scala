package org.validoc.utils.map

import java.util.concurrent.atomic.AtomicInteger

import org.validoc.utils.UtilsWithLoggingSpec

import scala.reflect.ClassTag


abstract class AbstractLowLevelSafeMapTest extends UtilsWithLoggingSpec {

  def makeMap(initial: => Integer): SafeMap[String, Int]

  behavior of getClass.getSimpleName

  def setup(fn: SafeMap[String, Int] => Unit) = {
    val count = new AtomicInteger
    val map = makeMap(count.incrementAndGet())
    fn(map)
  }

  it should "create a default value if doesn't exit, but return existing value if it does" in {
    setup { map =>
      map.get("one") shouldBe 1
      map.get("one") shouldBe 1
      map.get("two") shouldBe 2
      map.get("two") shouldBe 2
    }
  }

  it should "allow a value to be transformed" in {
    setup { map =>
      map("one")(_ + 1) shouldBe 2
      map("one")(_ + 1) shouldBe 3
      map("one")(_ + 1) shouldBe 4
      map("one")(_ + 1) shouldBe 5
    }
  }

  it should "allow a value to be transformed in a thread safe way" in {
    setup { map =>
      map.get("one")
      map.get("two")
      map.get("three")
      val threads = 1 to 999 map {
        _ =>
          new Thread() {
            override def run(): Unit = {
              map("one")(_ + 1)
              map("two")(_ + 1)
              map("three")(_ + 1)
            }
          }
      }
      threads.foreach(_.setDaemon(true))
      threads.foreach(_.start)
      eventually {
        threads.forall(!_.isAlive)
        withClue("one")(map.get("one") shouldBe 1000)
        withClue("two")( map.get("two") shouldBe 1001)
        withClue("three")(  map.get("three") shouldBe 1002)
      }
    }
  }
}

class MapOfAtomicRefSafeMapTest extends AbstractLowLevelSafeMapTest {
  override def makeMap(initial: => Integer): SafeMap[String, Int] = SafeMap(initial)
}