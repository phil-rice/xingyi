package one.xingyi.core.profiling

import java.util.concurrent.atomic.AtomicReference

import one.xingyi.core.UtilsSpec
import one.xingyi.core.functions.ScalaFutureAsAsyncAndMonadAndFailer
import one.xingyi.core.time.{MockTimeService, NanoTimeService}

import scala.concurrent.Future
import one.xingyi.core.language.Language._
import org.mockito.Mockito._

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
