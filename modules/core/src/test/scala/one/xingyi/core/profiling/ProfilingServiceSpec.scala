/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.profiling

import java.util.concurrent.atomic.AtomicReference

import one.xingyi.core.UtilsSpec
import one.xingyi.core.language.Language._
import one.xingyi.core.monad.ScalaFutureAsAsyncAndMonadAndFailer
import one.xingyi.core.time.{MockTimeService, NanoTimeService}

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

class ProfilingServiceSpec extends UtilsSpec with ScalaFutureAsAsyncAndMonadAndFailer {

  val runtimeException = new RuntimeException

  def setup(fn: (Int => Future[String], AtomicReference[(Long, Try[_])]) => Unit) = {
    implicit val mainNanoTimeService = new MockTimeService
    val ref = new AtomicReference[(Long, Try[_])]()
    val service = new ProfileKleisli[Future, Throwable] with ScalaFutureAsAsyncAndMonadAndFailer {
      override protected implicit def timeService: NanoTimeService = mainNanoTimeService
    }
    val profileData = new TryProfileData {
      override def eventFromStartTime[Res: ProfileAs](startTime: Long)(result: Try[Res])(implicit nanoTimeService: NanoTimeService) = {
        nanoTimeService shouldBe mainNanoTimeService
        ref.set((startTime, result))
      }
    }
    fn(service.profile(profileData) { x: Int => if (x == 0) throw runtimeException else Future.successful(x.toString) }, ref)

  }

  behavior of "Profile"

  it should "return the result of the delegate service when successful" in {
    setup { (service, ref) =>
      service(1).await shouldBe "1"
      ref.get shouldBe(1100L, Success("1"))
    }
  }
  it should "return the result of the delegate service when fails" in {
    setup { (service, ref) =>
      val m = service(0)
      intercept[RuntimeException](m.await()) shouldBe runtimeException
      ref.get shouldBe(1100L, Failure(runtimeException))
    }
  }


}
