package org.validoc.utils.profiling

import org.validoc.utils.functions.MonadWithException
import org.validoc.utils.language.Language._
import org.validoc.utils.time.NanoTimeService

import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

object ProfileKleisli {
  def apply[M[_] : MonadWithException, Req: ClassTag, Res: ClassTag](profileData: TryProfileData)(raw: Req => M[Res])(implicit timeService: NanoTimeService): Req => M[Res] =
    raw.onEnterAndExitM(_ => timeService(), profileData.eventFromStartTime)

}
trait ProfileKleisli[M[_], Fail] {
  protected implicit def monad: MonadWithException[M]
  protected implicit def timeService: NanoTimeService

  def profile[Req: ClassTag, Res: ClassTag](profileData: TryProfileData)(raw: Req => M[Res]): Req => M[Res] =ProfileKleisli(profileData)(raw)
//    raw.onEnterAndExitM(_ => timeService(), profileData.eventFromStartTime)
}

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


