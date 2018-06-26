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
