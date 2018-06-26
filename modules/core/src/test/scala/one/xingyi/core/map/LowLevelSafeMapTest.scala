/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.map

import java.util.concurrent.atomic.AtomicInteger

import org.scalatest.time.{Seconds, Span}
import one.xingyi.core.UtilsWithLoggingSpec


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
      eventually(timeout(Span(5, Seconds))) {
        threads.forall(!_.isAlive)
        withClue("one")(map.get("one") shouldBe 1000)
        withClue("two")(map.get("two") shouldBe 1001)
        withClue("three")(map.get("three") shouldBe 1002)
      }
    }
  }
}

class MapOfAtomicRefSafeMapTest extends AbstractLowLevelSafeMapTest {
  override def makeMap(initial: => Integer): SafeMap[String, Int] = SafeMap(initial)
}
