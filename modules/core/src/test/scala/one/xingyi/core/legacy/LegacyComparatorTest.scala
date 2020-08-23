/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.legacy

import java.util.Comparator

import one.xingyi.core.CoreSpec
import one.xingyi.core.monad.SuccessOrFail
import one.xingyi.core.language.AnyLanguage._
import one.xingyi.core.legacy.LegacyComparator.notEqualText
import scala.language.higherKinds
import scala.util.Try

abstract class AbstractLegacyComparatorTest[S[_] : SuccessOrFail] extends CoreSpec {

  val equals: LegacyComparator[S, String] = LegacyComparator.equals[S, String]
  implicit val c: Comparator[String] = (l, r) => l.compareTo(r)
  val comparator: LegacyComparator[S, String] = LegacyComparator.comparator[S, String]

  behavior of "LegacyComparator"

  it should "have an 'equals' that uses equals to compare and  handles exceptions" in {
    equals("one", "two".liftM) shouldBe List(Difference(notEqualText))
    equals("two", "one".liftM) shouldBe List(Difference(notEqualText))
    equals("two", "two".liftM) shouldBe List()
  }

  it should "have an 'comparator' that uses a comparator to compare and  handles exceptions" in {
    comparator("one", "two".liftM) shouldBe List(Difference(notEqualText))
    comparator("two", "one".liftM) shouldBe List(Difference(notEqualText))
    comparator("two", "two".liftM) shouldBe List()
  }

  it should "have a 'transform' comparator that transforms the two input before comparing them" in {
    val tx = LegacyComparator.withTransforms[S, String, String](_.substring(0, 3), _.substring(1, 4), equals)
    tx("123456", "01xx456".liftM[S]) shouldBe List(Difference(notEqualText))
    tx("1xx456", "0123456".liftM[S]) shouldBe List(Difference(notEqualText))
    tx("123456", "0123456".liftM[S]) shouldBe List()
  }
}

class TryLegacyComparatorTest extends AbstractLegacyComparatorTest[Try]
