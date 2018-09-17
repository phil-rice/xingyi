/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.builder
import one.xingyi.core.UtilsSpec

class AggregatorSpec extends UtilsSpec {

  //  trait HasAggregator[T] {
  //    def aggregator: Aggregator[T]
  //  }
  //  trait Aggregator[T] extends (T => Unit)
  //  object Aggregator{
  //    def nullAggregator [T]= new Aggregator[T] {
  //      override def apply(v1: T): Unit = {}
  //    }
  //  }
  //  trait HasId[T, ID] extends (T => ID)
  //
  //  class RememberingAggregator2[T,ID](implicit hasId: HasId[T, ID]) extends Aggregator[T] {
  //    private var list = List[T]()
  //    private val lock = new Object()
  //    def items = list
  //    override def apply(comp: T): Unit = list = list.filterNot(c => hasId(c) == hasId(comp)) :+ comp
  //    def clear = list = List()
  //  }


  behavior of "NullAggregator"

  it should "not crash: not much else can be said about it" in {
    val aggregator = Aggregator.nullAggregator[Int]
    aggregator(1)
    aggregator(1)
    aggregator(1)
  }

  behavior of "RememberingAggreator"

  case class IdAndName(id: Int, name: String)
  object IdAndName {
    implicit object HasIdForIdAndName extends HasId[IdAndName, Int] {
      override def apply(v1: IdAndName): Int = v1.id
    }
  }

  it should "remember the last data for each id" in {
    val aggregator = new RememberingAggregator2[IdAndName, Int]
    aggregator(IdAndName(1, "onea"))
    aggregator(IdAndName(1, "oneb"))
    aggregator(IdAndName(2, "twoa"))
    aggregator(IdAndName(2, "twob"))
    aggregator.items shouldBe List(IdAndName(1, "oneb"), IdAndName(2, "twob"))
  }
  it should "have a clear method and remember what is recorded after the clear" in {
    val aggregator = new RememberingAggregator2[IdAndName, Int]
    aggregator(IdAndName(1, "onea"))
    aggregator(IdAndName(1, "oneb"))
    aggregator.clear()
    aggregator(IdAndName(2, "twoa"))
    aggregator(IdAndName(2, "twob"))
    aggregator.items shouldBe List(IdAndName(2, "twob"))
  }

}
