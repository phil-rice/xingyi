/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddexamples
import one.xingyi.core.UtilsSpec



class TennisSpec extends UtilsSpec {


  behavior of "Tennis"

  it should "work" in {
    val t = new Tennis()
    import t._
    dump
     tennis(1, 1) shouldBe "fifteen all"
    tennis(2, 1) shouldBe "thirty, fifteen"
    tennis(3, 1) shouldBe "forty, fifteen"
    tennis(3, 2) shouldBe "forty, thirty"
    tennis(3, 4) shouldBe "advantage right"
    tennis(4, 4) shouldBe "deuce"
    tennis(4, 5) shouldBe "advantage right"
    tennis(5, 5) shouldBe "deuce"
    tennis(6, 5) shouldBe "advantage left"
    tennis(7, 5) shouldBe "left won"

    tennis(3, 3) shouldBe "deuce"
  }

}
