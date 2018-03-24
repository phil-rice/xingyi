package org.validoc.utils.profiling

import org.validoc.utils.functions.MonadWithException
import org.validoc.utils.language.Language._
import org.validoc.utils.time.NanoTimeService

import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

sealed trait ProfileState
case object ProfileAsSuccess extends ProfileState
case object ProfileAsFail extends ProfileState
case object DontProfile extends ProfileState
trait ProfileAs[T] extends (Try[T] => ProfileState)

object ProfileAs {
  implicit def profileAs[T] = new ProfileAs[T] {
    override def apply(tryT: Try[T]) = tryT match {
      case Success(_) => ProfileAsSuccess
      case _ => ProfileAsFail
    }
  }

}

object ProfileKleisli {
  def apply[M[_] : MonadWithException, Req: ClassTag, Res: ClassTag : ProfileAs](profileData: TryProfileData)(raw: Req => M[Res])(implicit timeService: NanoTimeService): Req => M[Res] =
    raw.onEnterAndExitM(_ => timeService(), profileData.eventFromStartTime)

}
trait ProfileKleisli[M[_], Fail] {
  protected implicit def monad: MonadWithException[M]
  protected implicit def timeService: NanoTimeService

  def profile[Req: ClassTag, Res: ClassTag : ProfileAs](profileData: TryProfileData)(raw: Req => M[Res]): Req => M[Res] = ProfileKleisli(profileData)(raw)
  //    raw.onEnterAndExitM(_ => timeService(), profileData.eventFromStartTime)
}

class TryProfileData {
  def clearData = {
    succeededData.clearData
    failedData.clearData
  }

  val succeededData = new ProfileData
  val failedData = new ProfileData

  def event[Res](nanos: Long)(result: Try[Res])(implicit profileAs: ProfileAs[Res]): Unit = {
    profileAs(result) match {
      case ProfileAsSuccess => succeededData.event(nanos)
      case ProfileAsFail => failedData.event(nanos)
      case DontProfile =>
    }
  }
  def eventFromStartTime[Res: ProfileAs](startTime: Long)(result: Try[Res])(implicit nanoTimeService: NanoTimeService): Unit =
    event(nanoTimeService() - startTime)(result)


  def toShortString = succeededData.shortToString + "  " + failedData.shortToString
}

trait ProfileInfo {
  def name: String

  def tryProfileData: TryProfileData
}


