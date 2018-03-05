package org.validoc.utils.profiling

import org.validoc.utils.time.NanoTimeService

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

class TryProfileData {
  def clearData = {
    succeededData.clearData
    failedData.clearData
  }

  val succeededData = new ProfileData
  val failedData = new ProfileData

  def event(nanos: Long)(result: Try[_]): Unit = {
    result match {
      case Success(_) => succeededData.event(nanos)
      case Failure(_) => failedData.event(nanos)
    }
  }
  def eventFromStartTime(startTime: Long)(result: Try[_])(implicit nanoTimeService: NanoTimeService): Unit =
    event(nanoTimeService() - startTime)(result)


  def toShortString = succeededData.shortToString + "  " + failedData.shortToString
}

trait ProfileInfo {
  def name: String

  def tryProfileData: TryProfileData
}


