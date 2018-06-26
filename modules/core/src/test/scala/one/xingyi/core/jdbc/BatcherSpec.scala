/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.jdbc
import one.xingyi.core.UtilsSpec

class BatcherSpec extends UtilsSpec {

  behavior of "Batcher"

  it should "batch things up into 'batchsize' and call flush every batchsize" in {
    var batchList = List[Int]()
    var resultList = List[List[Int]]()

    val batcher = new Batcher[Int](BatchConfig(3, l => batchList = batchList :+ l, { () => resultList = resultList :+ batchList; batchList = List() }))
    List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).foreach(batcher)
    resultList shouldBe List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
    batcher.close
    resultList shouldBe List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9), List(10))
  }

}
