package one.xingyi.utils.map

import scala.util.Random

trait ReportMapSizeReduction {
  def mapSizeChanges[K](oldSize: Int, keysRemoved: Iterable[K])
}

object NoReportMapSizeReduction extends ReportMapSizeReduction {
  override def mapSizeChanges[K](oldSize: Int, keysRemoved: Iterable[K]) {}
}

sealed trait MapSizeStrategy {
  def modifyCache[K, V](cache: Map[K, V], reportMapSizeChange: ReportMapSizeReduction): Map[K, V]
}


case class MaxMapSizeStrategy(maxSize: Int, numberToRemove: Int) extends MapSizeStrategy {
  require(maxSize > 0)
  require(numberToRemove > 0)

  val random = new Random()

  override def modifyCache[K, V](cache: Map[K, V], reportMapSizeChange: ReportMapSizeReduction): Map[K, V] = {
    if (cache.size < maxSize) cache else {
      val keys = cache.keys
      val firstToGo = Random.nextInt(keys.size - numberToRemove)
      val keysToGo = keys.drop(firstToGo).take(numberToRemove)
      reportMapSizeChange.mapSizeChanges(keys.size, keysToGo)
      keysToGo.foldLeft(cache)((acc, key) => acc - key)
    }
  }

}

object NoMapSizeStrategy extends MapSizeStrategy {
  override def modifyCache[K, V](cache: Map[K, V], reportMapSizeChange: ReportMapSizeReduction): Map[K, V] = cache

}

