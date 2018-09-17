package one.xingyi.core.streams
import one.xingyi.core.UtilsSpec
import one.xingyi.core.functions.Streams

class StreamsSpec extends UtilsSpec {

  behavior of "Streams"

  it should "unfold a simple function" in {
    val s = Streams.unfold(0)(s => Some((s.toString, s + 1)))
    s.take(10).toList shouldBe List("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
  }

  it should "unfold when new data comes in the form of a list (bit like a flatmap) using unfoldList" in {
    val s = Streams.unfoldList(0)(s => Some((List(s, s + 1, s + 2).map(_.toString), s + 10)))
    s.take(10).toList shouldBe List("0", "1", "2", "10", "11", "12", "20", "21", "22", "30")
  }

  it should "unfold when new data comes in the form of a list (bit like a flatmap) using unfoldIndexedList" in {
    val s = Streams.unfoldIndexedList(0, "")((n, s) => Some((List(n.toString, s), s + ".")))
    s.take(10).toList shouldBe List("0", "", "1", ".", "2", "..", "3", "...", "4", "....")

  }

}
