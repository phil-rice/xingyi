/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.profiling
import one.xingyi.core.language.Language._
import one.xingyi.core.monad.MonadWithException
import one.xingyi.core.time.NanoTimeService

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


class ProfileService[M[_] : MonadWithException, Req: ClassTag, Res: ClassTag : ProfileAs](val profileData: TryProfileData)(val delegate: Req => M[Res])(implicit timeService: NanoTimeService) extends (Req => M[Res]){
  override def apply(req: Req): M[Res] = delegate.onEnterAndExitM(_ => timeService(), profileData.eventFromStartTime) apply req
}

trait ProfileKleisli[M[_], Fail] {
  protected implicit def monad: MonadWithException[M]
  protected implicit def timeService: NanoTimeService

  def profile[Req: ClassTag, Res: ClassTag : ProfileAs](profileData: TryProfileData)(raw: Req => M[Res]): Req => M[Res] = new ProfileService(profileData)(raw)
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


