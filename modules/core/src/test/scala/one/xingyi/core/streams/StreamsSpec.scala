/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
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
