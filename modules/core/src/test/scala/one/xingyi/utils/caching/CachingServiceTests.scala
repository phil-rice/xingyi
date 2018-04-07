package one.xingyi.utils.caching

import java.util.concurrent.{CountDownLatch, Executors}

import org.scalatest.concurrent.Eventually
import one.xingyi.utils.UtilsWithLoggingSpec
import one.xingyi.utils.cache._
import one.xingyi.utils.map.{MapSizeStrategy, MaxMapSizeStrategy, NoMapSizeStrategy}
import one.xingyi.utils.time.NanoTimeService

import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds
import scala.util.{Failure, Success, Try}
import one.xingyi.utils.functions.AsyncForScalaFuture._
import ImplicitsForTest._
import one.xingyi.utils.functions.Async

case class DelegateRequest(key: String, result: Try[String], bypassCache: Boolean = false, countDownLatch: CountDownLatch = new CountDownLatch(1)) {
  override def hashCode(): Int = key.hashCode

  override def equals(obj: scala.Any): Boolean = obj match {
    case DelegateRequest(key, _, _, _) if key == this.key => true
    case _ => false
  }
}

object DelegateRequest {

  implicit object CachableKeyForDelegateRequest extends CachableKey[DelegateRequest] {
    override def id(req: DelegateRequest): Id = StringId(req.key)

    override def bypassCache(req: DelegateRequest): Boolean = req.bypassCache
  }

}

class DelegateService[M[_] : Async] extends (DelegateRequest => M[String]) {

  def apply(req: DelegateRequest): M[String] = implicitly[Async[M]].async {
    req.countDownLatch.await()
    req.result.get
  }
}


class CachingServiceTests extends UtilsWithLoggingSpec with Eventually {

  import org.mockito.Mockito._

  val lock = new Object

  type CService = CachingService[Future, DelegateRequest, String]

  def withServices(fn: CService => DelegateService[Future] => NanoTimeService => Unit, cacheSizeStrategy: MapSizeStrategy = NoMapSizeStrategy): Unit =
    lock.synchronized {
      val threadPool = Executors.newFixedThreadPool(10)
      implicit val ec = ExecutionContext.fromExecutor(threadPool)
      val delegateService = new DelegateService[Future]()
      implicit val timeService = mock[NanoTimeService]
      val cachingStrategy = DurationStaleCacheStategy(100, 1000)
      val cachingServiceFactory = new CachingServiceFactory[Future](cachingStrategy, cacheSizeStrategy)
      val cachingKleisli = new CacheKleisli[Future] {
        override protected def cacheFactory = cachingServiceFactory
      }
      val cachingService = (cachingKleisli.cache[DelegateRequest, String]("someName")(delegateService)).asInstanceOf[CachingService[Future, DelegateRequest, String]]
      //       = new CachingService[Future, DelegateRequest, String]("someName", delegateService, cachingStrategy, cacheSizeStrategy)
      fn(cachingService)(delegateService)(timeService)
      threadPool.shutdown()
    }

  def checkMetrics(prefix: String, bypassedRequest: Long = 0,
                   requests: Long = 0,
                   staleRequests: Long = 0,
                   deadRequests: Long = 0,
                   delegateSuccesses: Long = 0,
                   delegateRequests: Long = 0,
                   delegateFailures: Long = 0,
                   inTransits: Option[Long] = None,
                   inTransitWithoutResults: Option[Long] = None,
                   cacheSize: Option[Int] = None,
                   removedBecauseTooFull: Long = 0
                  )(implicit cachingService: CService): Unit =
    eventually {
      val metrics = cachingService.cachingMetrics
      val fullPrefix = "\n" + prefix + s"\n" + s"\n$metrics\n"
      withClue(s"$fullPrefix bypassedRequests")(metrics.bypassedRequests shouldBe bypassedRequest)
      withClue(s"$fullPrefix requests")(metrics.requests shouldBe requests)
      withClue(s"$fullPrefix staleRequest")(metrics.staleRequest shouldBe staleRequests)
      withClue(s"$fullPrefix deadRequests")(metrics.deadRequest shouldBe deadRequests)
      withClue(s"$fullPrefix delegateFailures")(metrics.delegateFailures shouldBe delegateFailures)
      withClue(s"$fullPrefix delegateRequests")(metrics.delegateRequests shouldBe delegateRequests)
      withClue(s"$fullPrefix delegateSuccesses")(metrics.delegateSuccesses shouldBe delegateSuccesses)
      withClue(s"$fullPrefix removedBecauseTooFull")(metrics.removedBecauseTooFull shouldBe removedBecauseTooFull)
      inTransits.foreach(i => withClue(s"$fullPrefix inTransits")(metrics.inTransits shouldBe i))
      inTransitWithoutResults.foreach(i => withClue(s"$fullPrefix inTransitWithoutResults")(metrics.inTransitWithoutResults shouldBe i))
      cacheSize.foreach(i => withClue(s"$fullPrefix cacheSize")(metrics.cacheSize shouldBe i))
    }


  behavior of "CachedValue"

  it should "should blow up if you try and get the future value" in {
    val cv = CachedValue(1, CachedId(123), None, None)
    intercept[RuntimeException](cv.valueToUse).getMessage shouldBe "Should not get this. trying to return future from CachedValue"

  }


  behavior of "CachingService"

  it should "bypass the cache if the request has caching properties set to do not cache" in {
    withServices { implicit cachingService =>
      delegateService =>
        timeService =>
          val request = DelegateRequest("key", Success("result"), bypassCache = true)
          val future: Future[String] = cachingService(request)
          request.countDownLatch.countDown()
          await(future) shouldBe "result"
          checkMetrics("", bypassedRequest = 1, inTransitWithoutResults = Some(0))
    }
  }


  it should "Add the first ever intransit for a request to the caching one.xingyi.utils.map" in {
    withServices { implicit cachingService =>
      delegateService =>
        timeService =>
          val request = DelegateRequest("key", Success("result"))
          val future = cachingService(request)
          checkMetrics("Before countdown", requests = 1, delegateRequests = 1, inTransitWithoutResults = Some(1), inTransits = Some(1))
          request.countDownLatch.countDown()
          eventually(future.isCompleted)
          withClue("waiting to finish")(await(future) shouldBe "result")
          checkMetrics("After awaiting", requests = 1, delegateRequests = 1, delegateSuccesses = 1)
    }
  }

  it should "share that future with a second request made before the transit has finished" in {
    withServices { implicit cachingService =>
      delegateService =>
        timeService =>
          val request1 = DelegateRequest("sameKey", Success("result1"))
          val request2 = DelegateRequest("sameKey", Success("result2"))

          val future1 = cachingService(request1)
          val future2 = cachingService(request2)
          checkMetrics("before countdown", requests = 2, delegateRequests = 1, inTransits = Some(1))
          request1.countDownLatch.countDown()
          await(future2) shouldBe "result1" //from the future of request1
          checkMetrics("After awaiting", requests = 2, delegateRequests = 1, delegateSuccesses = 1, inTransits = Some(0))
    }
  }

  it should "share the result with a second request made after the transit has finished" in {
    withServices { implicit cachingService =>
      delegateService =>
        timeService =>
          val request1 = DelegateRequest("sameKey", Success("result1"))
          val request2 = DelegateRequest("sameKey", Success("result2"))
          request1 shouldEqual request2
          val future1 = cachingService(request1)
          request1.countDownLatch.countDown()
          await(future1) shouldBe "result1" //making sure that future1 has finished

          checkMetrics("after future 1 finished ", requests = 1, delegateRequests = 1, delegateSuccesses = 1)
          val future2 = cachingService(request2)
          checkMetrics("after future 1 finished and future 2 started", requests = 2, delegateRequests = 1, delegateSuccesses = 1)
          await(future2) shouldBe "result1" //from the future of request1
          checkMetrics("After awaiting for future2", requests = 2, delegateRequests = 1, delegateSuccesses = 1)
    }
  }

  it should "launch a new intransit if a second request implies staleness, but return the stale value" in {
    withServices { implicit cachingService =>
      delegateService =>
        timeService =>
          val request1 = DelegateRequest("sameKey", Success("result1"))
          val request2 = DelegateRequest("sameKey", Success("result2"))

          val future1 = cachingService(request1)
          request1.countDownLatch.countDown()
          await(future1) shouldBe "result1"
          checkMetrics("after future 1 finished", requests = 1, delegateRequests = 1, delegateSuccesses = 1)

          when(timeService.apply()) thenReturn 200

          val future2 = cachingService(request2)
          request2.countDownLatch.countDown()
          checkMetrics("after future 1&2 finished", requests = 2, delegateRequests = 2, delegateSuccesses = 2, staleRequests = 1)

          val future3 = cachingService(request2)
          await(future3) shouldBe "result2"
          checkMetrics("after future 1, and 3", requests = 3, delegateRequests = 2, delegateSuccesses = 2, staleRequests = 1)

    }
  }

  it should "launch a new intransit if a second request implies staleness. If a third one is made after the intransit finishes it should get the new value" in {
    withServices { implicit cachingService =>
      delegateService =>
        timeService =>
          val request1 = DelegateRequest("sameKey", Success("result1"))
          val request2 = DelegateRequest("sameKey", Success("result2"))
          val request3 = DelegateRequest("sameKey", Success("result3"))

          val future1 = cachingService(request1)
          checkMetrics("after future 1 started ", requests = 1, delegateRequests = 1, delegateSuccesses = 0)
          request1.countDownLatch.countDown()
          await(future1) shouldBe "result1"
          checkMetrics("after future 1 finished ", requests = 1, delegateRequests = 1, delegateSuccesses = 1)

          when(timeService.apply()) thenReturn 200

          val future2 = cachingService(request2)
          checkMetrics("after future 1 finished and future 2 started", requests = 2, delegateRequests = 2, delegateSuccesses = 1, staleRequests = 1)
          await(future2) shouldBe "result1"

          request2.countDownLatch.countDown()
          checkMetrics("after future2 finished ", requests = 2, delegateRequests = 2, delegateSuccesses = 2, staleRequests = 1)

          val future3 = cachingService(request3)
          checkMetrics("after future3 started ", requests = 3, delegateRequests = 2, delegateSuccesses = 2, staleRequests = 1)
          request3.countDownLatch.countDown()
          await(future2) shouldBe "result1" //this was served while stale
          await(future3) shouldBe "result2"

          checkMetrics("after future3 finished", requests = 3, delegateRequests = 2, delegateSuccesses = 2, staleRequests = 1)
    }
  }

  it should "not launch lots of transits  when one is already refreshing the staleness" in {
    withServices { implicit cachingService =>
      delegateService =>
        timeService =>
          val request1 = DelegateRequest("sameKey", Success("result1"))
          val request2 = DelegateRequest("sameKey", Success("result2"))
          val request3 = DelegateRequest("sameKey", Success("result3"))
          val request4 = DelegateRequest("sameKey", Success("result4"))

          val future1 = cachingService(request1)
          request1.countDownLatch.countDown()
          await(future1) shouldBe "result1"
          checkMetrics("after future 1 finished ", requests = 1, delegateRequests = 1, delegateSuccesses = 1)

          when(timeService.apply()) thenReturn 200

          val future2 = cachingService(request2)
          checkMetrics("after future 1 finished and future 2 started. ", requests = 2, delegateRequests = 2, delegateSuccesses = 1, staleRequests = 1)

          val future3 = cachingService(request3)
          checkMetrics("after future3 started", requests = 3, delegateRequests = 2, delegateSuccesses = 1, staleRequests = 2)

          request2.countDownLatch.countDown()
          request3.countDownLatch.countDown()
          await(future2) shouldBe "result1"
          await(future3) shouldBe "result1"
          checkMetrics("after future3 finished", requests = 3, delegateRequests = 2, delegateSuccesses = 2, staleRequests = 2)

          val future4 = cachingService(request4)
          request4.countDownLatch.countDown()
          await(future4) shouldBe "result2"
          checkMetrics("after future3 finished", requests = 4, delegateRequests = 2, delegateSuccesses = 2, staleRequests = 2)
    }
  }

  it should "launch a new transit if the first request failed" in {
    withServices { implicit cachingService =>
      delegateService =>
        timeService =>
          val request1 = DelegateRequest("sameKey", Failure(new RuntimeException))
          val request2 = DelegateRequest("sameKey", Success("result2"))

          val future1 = cachingService(request1)
          request1.countDownLatch.countDown()
          intercept[RuntimeException](await(future1))
          checkMetrics("after future 1 finished ", requests = 1, delegateRequests = 1, delegateFailures = 1)

          val future2 = cachingService(request2)
          checkMetrics("after future 1 finished and future 2 started", requests = 2, delegateRequests = 2, delegateFailures = 1)
          request2.countDownLatch.countDown()
          await(future2) shouldBe "result2"
          checkMetrics("After awaiting for future2", requests = 2, delegateRequests = 2, delegateSuccesses = 1, delegateFailures = 1)
    }
  }


  it should "allow requests have a intransit has failed to try again, still serving up stale values" in {
    withServices { implicit cachingService =>
      delegateService =>
        timeService =>
          val request1 = DelegateRequest("sameKey", Success("result1"))
          val request2 = DelegateRequest("sameKey", Failure(new RuntimeException))
          val request3 = DelegateRequest("sameKey", Success("result3"))
          val request4 = DelegateRequest("sameKey", Success("result4"))

          cachingService(request1)
          request1.countDownLatch.countDown()
          checkMetrics("after future 1 finished ", requests = 1, delegateRequests = 1, delegateSuccesses = 1)

          when(timeService.apply()) thenReturn 200

          val future2 = cachingService(request2)
          checkMetrics("after future 1 finished and future 2 started", requests = 2, delegateRequests = 2, delegateSuccesses = 1, delegateFailures = 0, staleRequests = 1)
          request2.countDownLatch.countDown()
          checkMetrics("after future 1 finished and future 2 finish", requests = 2, delegateRequests = 2, delegateSuccesses = 1, delegateFailures = 1, staleRequests = 1)
          await(future2) shouldBe "result1"

          val future3 = cachingService(request3)
          await(future3) shouldBe "result1" //this is actually stale and the result of the first request
          checkMetrics("after future 1&2 finished and future 3 started", requests = 3, delegateRequests = 3, delegateSuccesses = 1, delegateFailures = 1, staleRequests = 2)
          request3.countDownLatch.countDown()
          checkMetrics("After awaiting for future3", requests = 3, delegateRequests = 3, delegateSuccesses = 2, delegateFailures = 1, staleRequests = 2)

          val future4 = cachingService(request4)
          checkMetrics("after future 1&2&3 finished and future 4 started", requests = 4, delegateRequests = 3, delegateSuccesses = 2, delegateFailures = 1, staleRequests = 2)
          await(future4) shouldBe "result3"
          checkMetrics("After awaiting for future4", requests = 4, delegateRequests = 3, delegateSuccesses = 2, delegateFailures = 1, staleRequests = 2)
    }
  }

  it should "not use data from a transit after cache has been cleared" in {
    withServices { implicit cachingService =>
      delegateService =>
        timeService =>
          val request1 = DelegateRequest("sameKey", Success("result1"))
          val request2 = DelegateRequest("sameKey", Success("result2"))

          val future1 = cachingService(request1)
          checkMetrics("after future 1 started ", requests = 1, delegateRequests = 1, delegateSuccesses = 0, cacheSize = Some(1))
          cachingService.clear
          checkMetrics("after clearCache", requests = 1, delegateRequests = 1, delegateSuccesses = 0, cacheSize = Some(0))
          request1.countDownLatch.countDown()
          await(future1) shouldBe "result1"
          checkMetrics("after future 1 finished ", requests = 1, delegateRequests = 1, delegateSuccesses = 1, cacheSize = Some(1))

          val future2 = cachingService(request2)
          checkMetrics("after future 2 started ", requests = 2, delegateRequests = 2, delegateSuccesses = 1, cacheSize = Some(1))

          request2.countDownLatch.countDown()
          checkMetrics("after future2 finished", requests = 2, delegateRequests = 2, delegateSuccesses = 2)
          await(future2) shouldBe "result2"
    }
  }
  it should "not use data from a transit after cache has been cleared, and another request already sent" in {
    withServices { implicit cachingService =>
      delegateService =>
        timeService =>
          val request1 = DelegateRequest("sameKey", Success("result1"))
          val request2 = DelegateRequest("sameKey", Success("result2"))
          val request3 = DelegateRequest("sameKey", Success("result3"))

          val future1 = cachingService(request1)
          checkMetrics("after future 1 started ", requests = 1, delegateRequests = 1, delegateSuccesses = 0, cacheSize = Some(1))
          cachingService.clear

          val future2 = cachingService(request2)
          checkMetrics("after future 2 started ", requests = 2, delegateRequests = 2, delegateSuccesses = 0, cacheSize = Some(1))

          request1.countDownLatch.countDown()
          await(future1) shouldBe "result1"
          checkMetrics("after future 1 finished ", requests = 2, delegateRequests = 2, delegateSuccesses = 1, cacheSize = Some(1))

          request2.countDownLatch.countDown()
          checkMetrics("after future2 finished", requests = 2, delegateRequests = 2, delegateSuccesses = 2)
          await(future2) shouldBe "result2"

          val future3 = cachingService(request3)
          request3.countDownLatch.countDown()
          checkMetrics("future 3 finished", requests = 3, delegateRequests = 2, delegateSuccesses = 2)
          await(future3) shouldBe "result2"
    }
  }

  it should "note a failing outstanding request to mess things up after a clearcache" in {
    withServices(cacheSizeStrategy = MaxMapSizeStrategy(3, 2), fn = { implicit cachingService =>
      delegateService =>
        timeService =>
          val request1 = DelegateRequest("sameKey", Failure(new RuntimeException))
          val request2 = DelegateRequest("sameKey", Success("result2"))
          val request3 = DelegateRequest("sameKey", Success("result3"))

          val future1 = cachingService(request1)
          cachingService.clear
          checkMetrics("after future 1 started ", requests = 1, delegateRequests = 1, delegateFailures = 0, cacheSize = Some(0))
          request1.countDownLatch.countDown()
          checkMetrics("after future 1 finished ", requests = 1, delegateRequests = 1, delegateFailures = 1, cacheSize = Some(1))

          val future2 = cachingService(request2)
          request2.countDownLatch.countDown()
          checkMetrics("after future 2 started ", requests = 2, delegateRequests = 2, delegateFailures = 1, delegateSuccesses = 1, cacheSize = Some(1))
          await(future2) shouldBe "result2"
    })
  }


  it should "have a time to live, and after that time any queries blow away the cache and start again" in {
    withServices {
      implicit cachingService =>
        delegateService =>
          timeService =>
            val request1 = DelegateRequest("sameKey", Success("result1"))
            val request2 = DelegateRequest("sameKey", Success("result2"))

            val future1 = cachingService(request1)

            checkMetrics("after future 1 started ", requests = 1, delegateRequests = 1, cacheSize = Some(1))
            request1.countDownLatch.countDown()
            checkMetrics("after future 1 finished ", requests = 1, delegateRequests = 1, delegateSuccesses = 1, cacheSize = Some(1))

            when(timeService.apply()) thenReturn 1200

            val future2 = cachingService(request2)

            checkMetrics("after future 2 started ", requests = 2, delegateRequests = 2, delegateSuccesses = 1, deadRequests = 1, cacheSize = Some(1))

            future2.isCompleted shouldBe false
            request2.countDownLatch.countDown()
            checkMetrics("after future 2 finished ", requests = 2, delegateRequests = 2, delegateSuccesses = 2, deadRequests = 1, cacheSize = Some(1))
            await(future2) shouldBe "result2"
    }
  }

  it should "have a maximum size and randomly delete items if the maximum size is exceeded" in {
    withServices(cacheSizeStrategy = MaxMapSizeStrategy(4, 2), fn = {
      implicit cachingService =>
        delegateService =>
          timeService =>
            val request1 = DelegateRequest("key1", Success("result1"))
            val request2 = DelegateRequest("key2", Success("result2"))
            val request3 = DelegateRequest("key3", Success("result3"))
            val request4 = DelegateRequest("key4", Success("result4"))
            val request5 = DelegateRequest("key5", Success("result5"))
            val request6 = DelegateRequest("key6", Success("result6"))
            val request7 = DelegateRequest("key7", Success("result7"))

            def addAndCheck(request: DelegateRequest): Unit = {
              val future1 = cachingService(request)
              val future2 = cachingService(request)
              future1 shouldBe future2 //the last thing added should be in, others might not
              request.countDownLatch.countDown()
            }

            addAndCheck(request1)
            checkMetrics("after 1", requests = 2, delegateRequests = 1, delegateSuccesses = 1, cacheSize = Some(1))
            addAndCheck(request2)
            checkMetrics("after 2", requests = 4, delegateRequests = 2, delegateSuccesses = 2, cacheSize = Some(2))
            addAndCheck(request3)
            checkMetrics("after 3", requests = 6, delegateRequests = 3, delegateSuccesses = 3, cacheSize = Some(3))
            addAndCheck(request4)
            checkMetrics("after 4", requests = 8, delegateRequests = 4, delegateSuccesses = 4, cacheSize = Some(4))
            addAndCheck(request5)
            checkMetrics("after 5", requests = 10, delegateRequests = 5, delegateSuccesses = 5, cacheSize = Some(3), removedBecauseTooFull = 2)
            addAndCheck(request6)
            checkMetrics("after 6", requests = 12, delegateRequests = 6, delegateSuccesses = 6, cacheSize = Some(4), removedBecauseTooFull = 2)
            addAndCheck(request7)
            checkMetrics("after 7", requests = 14, delegateRequests = 7, delegateSuccesses = 7, cacheSize = Some(3), removedBecauseTooFull = 4)

    })
  }
}