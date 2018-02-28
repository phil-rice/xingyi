package org.validoc.utils.profiling

import org.validoc.utils.Service
import org.validoc.utils.functions.{Monad, MonadWithException}
import org.validoc.utils.time.{NanoTimeService, SystemClockNanoTimeService}

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}
import org.validoc.utils._

class TryProfileData {
  def clearData = {
    succeededData.clearData
    failedData.clearData
  }

  val succeededData = new ProfileData
  val failedData = new ProfileData

  def event(nanos: Long)(result: Try[_]): Unit = result match {
    case Success(_) => succeededData.event(nanos)
    case Failure(_) => failedData.event(nanos)
  }
}

trait ProfileInfo {
  def name: String

  def tryProfileData: TryProfileData
}


