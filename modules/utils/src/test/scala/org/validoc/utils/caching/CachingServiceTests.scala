package org.validoc.utils.caching

import java.util.concurrent.{CountDownLatch, Executors}

import org.scalatest.concurrent.Eventually
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{FlatSpec, Matchers}
import org.validoc.utils.Futurable
import org.validoc.utils.map.{MapSizeStrategy, MaxMapSizeStrategy, NoMapSizeStrategy, NoMapSizeStrategy$}
import org.validoc.utils.time.{NanoTimeService, SystemClockNanoTimeService}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}


case class DelegateRequest(key: String, result: Try[String], bypassCache: Boolean = false, countDownLatch: CountDownLatch = new CountDownLatch(1)) {
  override def hashCode(): Int = key.hashCode

  override def equals(obj: scala.Any): Boolean = obj match {
    case DelegateRequest(key, _, _, _) if key == this.key => true
    case _ => false
  }
}

class DelegateService[M[_] : Futurable] extends (DelegateRequest => M[String]) {

  def apply(req: DelegateRequest): M[String] = implicitly[Futurable[M]].launch {
    req.countDownLatch.await()
    req.result.get
  }
}

object DelegateRequest {

  implicit object DelegateRequestCachingProperties extends CachingProperties[DelegateRequest] {
    override def bypassCache(t: DelegateRequest): Boolean = t.bypassCache
  }

}

class CachingServiceTests extends FlatSpec with Matchers with MockitoSugar with Eventually {

  import org.mockito.Mockito._


  val lock = new Object

  def withServices(fn: CachingService[Future, Throwable, DelegateRequest, String] => DelegateService[Future] => NanoTimeService => Unit, cacheSizeStrategy: MapSizeStrategy = NoMapSizeStrategy): Unit =
    lock.synchronized {
      implicit val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(10))
      val delegateService = new DelegateService[Future]()
      val timeService = mock[NanoTimeService]
      val cachingConfig = CachingConfig[Future](timeService, DurationStaleCacheStategy(100, 1000))
      val cachingConfigWithNameAndSize = new CachingConfigWithNameAndSize[Future](cachingConfig, "cachingServiceTests", cacheSizeStrategy)
      val cachingService = new CachingService[Future, Throwable, DelegateRequest, String](delegateService, cachingConfigWithNameAndSize)
      fn(cachingService)(delegateService)(timeService)
    }

  def checkMetrics(prefix: String, bypassedRequest: Long = 0,
                   requests: Long = 0,
                   staleRequests: Long = 0,
                   deadRequests: Long = 0,
                   inTransitSucesses: Long = 0,
                   inTransitRequests: Long = 0,
                   inTransitFailures: Long = 0,
                   inTransits: Option[Long] = None,
                   inTransitWithoutResults: Option[Long] = None,
                   cacheSize: Option[Int] = None,
                   removedBecauseTooFull: Long = 0
                  )(implicit cachingService: CachingService[Future, Throwable,  DelegateRequest, String]): Unit =
    eventually {
      val metrics = cachingService.cachingMetrics
      val fullPrefix = "\n" + prefix + s"\n"  + s"\n$metrics\n"
      withClue(s"$fullPrefix bypassedRequests")(metrics.bypassedRequests shouldBe bypassedRequest)
      withClue(s"$fullPrefix requests")(metrics.requests shouldBe requests)
      withClue(s"$fullPrefix staleRequest")(metrics.staleRequest shouldBe staleRequests)
      withClue(s"$fullPrefix deadRequests")(metrics.deadRequest shouldBe deadRequests)
      withClue(s"$fullPrefix inTransitFailures")(metrics.inTransitFailures shouldBe inTransitFailures)
      withClue(s"$fullPrefix inTransitRequests")(metrics.inTransitRequests shouldBe inTransitRequests)
      withClue(s"$fullPrefix inTransitSucesses")(metrics.inTransitSucesses shouldBe inTransitSucesses)
      withClue(s"$fullPrefix removedBecauseTooFull")(metrics.removedBecauseTooFull shouldBe removedBecauseTooFull)
      inTransits.foreach(i => withClue(s"$fullPrefix inTransits")(metrics.inTransits shouldBe i))
      inTransitWithoutResults.foreach(i => withClue(s"$fullPrefix inTransitWithoutResults")(metrics.inTransitWithoutResults shouldBe i))
      cacheSize.foreach(i => withClue(s"$fullPrefix cacheSize")(metrics.cacheSize shouldBe i))
    }


  "CachingService" should "bypass the cache if the request has caching properties set to do not cache" in {
    withServices { implicit cachingService =>
      delegateService =>
        timeService =>
          val request = DelegateRequest("key", Success("result"), bypassCache = true)
          val future: Future[String] = cachingService(request)
          request.countDownLatch.countDown()
          Await.result(future, 5 seconds) shouldBe "result"
          checkMetrics("", bypassedRequest = 1, inTransitWithoutResults = Some(0))
    }
  }

  it should "have a name which is the simple name of the result class" in {
    withServices { implicit cachingService =>
      delegateService =>
        timeService =>
          cachingService.name shouldBe "cachingServiceTests"
    }
  }

  it should "Add the first ever intransit for a request to the caching map" in {
    withServices { implicit cachingService =>
      delegateService =>
        timeService =>
          val request = DelegateRequest("key", Success("result"))
          val future = cachingService(request)
          checkMetrics("Before countdown", requests = 1, inTransitRequests = 1, inTransitWithoutResults = Some(1), inTransits = Some(1))
          request.countDownLatch.countDown()
          Await.result(future, 5 seconds) shouldBe "result"
          checkMetrics("After awaiting", requests = 1, inTransitRequests = 1, inTransitSucesses = 1)
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
          checkMetrics("before countdown", requests = 2, inTransitRequests = 1, inTransits = Some(1))
          request1.countDownLatch.countDown()
          Await.result(future2, 5 seconds) shouldBe "result1" //from the future of request1
          checkMetrics("After awaiting", requests = 2, inTransitRequests = 1, inTransitSucesses = 1, inTransits = Some(0))
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
          Await.result(future1, 5 seconds) shouldBe "result1" //making sure that future1 has finished

          checkMetrics("after future 1 finished ", requests = 1, inTransitRequests = 1, inTransitSucesses = 1)
          val future2 = cachingService(request2)
          checkMetrics("after future 1 finished and future 2 started", requests = 2, inTransitRequests = 1, inTransitSucesses = 1)
          Await.result(future2, 5 seconds) shouldBe "result1" //from the future of request1
          checkMetrics("After awaiting for future2", requests = 2, inTransitRequests = 1, inTransitSucesses = 1)
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
          Await.result(future1, 5 seconds) shouldBe "result1"
          checkMetrics("after future 1 finished", requests = 1, inTransitRequests = 1, inTransitSucesses = 1)

          when(timeService.apply()) thenReturn 200

          val future2 = cachingService(request2)
          request2.countDownLatch.countDown()
          checkMetrics("after future 1&2 finished", requests = 2, inTransitRequests = 2, inTransitSucesses = 2, staleRequests = 1)

          val future3 = cachingService(request2)
          Await.result(future3, 5 seconds) shouldBe "result2"
          checkMetrics("after future 1, and 3", requests = 3, inTransitRequests = 2, inTransitSucesses = 2, staleRequests = 1)

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
          request1.countDownLatch.countDown()
          Await.result(future1, 5 seconds) shouldBe "result1"
          checkMetrics("after future 1 finished ", requests = 1, inTransitRequests = 1, inTransitSucesses = 1)

          when(timeService.apply()) thenReturn 200

          val future2 = cachingService(request2)
          checkMetrics("after future 1 finished and future 2 started", requests = 2, inTransitRequests = 2, inTransitSucesses = 1, staleRequests = 1)
          Await.result(future2, 5 seconds) shouldBe "result1"

          request2.countDownLatch.countDown()
          checkMetrics("after future2 finished", requests = 2, inTransitRequests = 2, inTransitSucesses = 2, staleRequests = 1)
          val future3 = cachingService(request3)
          Await.result(future3, 5 seconds) shouldBe "result2"
          checkMetrics("after future3 finished", requests = 3, inTransitRequests = 2, inTransitSucesses = 2, staleRequests = 1)
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
          Await.result(future1, 5 seconds) shouldBe "result1"
          checkMetrics("after future 1 finished ", requests = 1, inTransitRequests = 1, inTransitSucesses = 1)

          when(timeService.apply()) thenReturn 200

          val future2 = cachingService(request2)
          checkMetrics("after future 1 finished and future 2 started. ", requests = 2, inTransitRequests = 2, inTransitSucesses = 1, staleRequests = 1)

          val future3 = cachingService(request3)
          checkMetrics("after future3 started", requests = 3, inTransitRequests = 2, inTransitSucesses = 1, staleRequests = 2)

          request2.countDownLatch.countDown()
          request3.countDownLatch.countDown()
          Await.result(future2, 5 seconds) shouldBe "result1"
          Await.result(future3, 5 seconds) shouldBe "result1"
          checkMetrics("after future3 finished", requests = 3, inTransitRequests = 2, inTransitSucesses = 2, staleRequests = 2)

          val future4 = cachingService(request4)
          request4.countDownLatch.countDown()
          Await.result(future4, 5 seconds) shouldBe "result2"
          checkMetrics("after future3 finished", requests = 4, inTransitRequests = 2, inTransitSucesses = 2, staleRequests = 2)
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
          intercept[RuntimeException](Await.result(future1, 5 seconds))
          checkMetrics("after future 1 fiCnished ", requests = 1, inTransitRequests = 1, inTransitFailures = 1)

          val future2 = cachingService(request2)
          checkMetrics("after future 1 finished and future 2 started", requests = 2, inTransitRequests = 2, inTransitFailures = 1)
          request2.countDownLatch.countDown()
          Await.result(future2, 5 seconds) shouldBe "result2"
          checkMetrics("After awaiting for future2", requests = 2, inTransitRequests = 2, inTransitSucesses = 1, inTransitFailures = 1)
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
          checkMetrics("after future 1 finished ", requests = 1, inTransitRequests = 1, inTransitSucesses = 1)

          when(timeService.apply()) thenReturn 200

          val future2 = cachingService(request2)
          request2.countDownLatch.countDown()
          checkMetrics("after future 1 finished and future 2 started", requests = 2, inTransitRequests = 2, inTransitSucesses = 1, inTransitFailures = 1, staleRequests = 1)
          Await.result(future2, 5 seconds) shouldBe "result1"

          val future3 = cachingService(request3)
          checkMetrics("after future 1&2 finished and future 3 started", requests = 3, inTransitRequests = 3, inTransitSucesses = 1, inTransitFailures = 1, staleRequests = 2)
          request3.countDownLatch.countDown()
          Await.result(future3, 5 seconds) shouldBe "result1"
          checkMetrics("After awaiting for future3", requests = 3, inTransitRequests = 3, inTransitSucesses = 2, inTransitFailures = 1, staleRequests = 2)

          val future4 = cachingService(request4)
          checkMetrics("after future 1&2&3 finished and future 4 started", requests = 4, inTransitRequests = 3, inTransitSucesses = 2, inTransitFailures = 1, staleRequests = 2)
          request4.countDownLatch.countDown()
          Await.result(future4, 5 seconds) shouldBe "result3"
          checkMetrics("After awaiting for future4", requests = 4, inTransitRequests = 3, inTransitSucesses = 2, inTransitFailures = 1, staleRequests = 2)
    }
  }

  it should "not use data from a transit after cache has been cleared" in {
    withServices { implicit cachingService =>
      delegateService =>
        timeService =>
          val request1 = DelegateRequest("sameKey", Success("result1"))
          val request2 = DelegateRequest("sameKey", Success("result2"))

          val future1 = cachingService(request1)
          checkMetrics("after future 1 started ", requests = 1, inTransitRequests = 1, inTransitSucesses = 0, cacheSize = Some(1))
          cachingService.clearCache
          request1.countDownLatch.countDown()
          Await.result(future1, 5 seconds) shouldBe "result1"
          checkMetrics("after future 1 finished ", requests = 1, inTransitRequests = 1, inTransitSucesses = 1, cacheSize = Some(0))

          val future2 = cachingService(request2)
          checkMetrics("after future 2 started ", requests = 2, inTransitRequests = 2, inTransitSucesses = 1, cacheSize = Some(1))

          request2.countDownLatch.countDown()
          checkMetrics("after future2 finished", requests = 2, inTransitRequests = 2, inTransitSucesses = 2)
          Await.result(future2, 5 seconds) shouldBe "result2"
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
          checkMetrics("after future 1 started ", requests = 1, inTransitRequests = 1, inTransitSucesses = 0, cacheSize = Some(1))
          cachingService.clearCache

          val future2 = cachingService(request2)
          checkMetrics("after future 2 started ", requests = 2, inTransitRequests = 2, inTransitSucesses = 0, cacheSize = Some(1))

          request1.countDownLatch.countDown()
          Await.result(future1, 5 seconds) shouldBe "result1"
          checkMetrics("after future 1 finished ", requests = 2, inTransitRequests = 2, inTransitSucesses = 1, cacheSize = Some(1))

          request2.countDownLatch.countDown()
          checkMetrics("after future2 finished", requests = 2, inTransitRequests = 2, inTransitSucesses = 2)
          Await.result(future2, 5 seconds) shouldBe "result2"

          val future3 = cachingService(request3)
          request3.countDownLatch.countDown()
          checkMetrics("future 3 finished", requests = 3, inTransitRequests = 2, inTransitSucesses = 2)
          Await.result(future3, 5 seconds) shouldBe "result2"
    }
  }

  it should "note a failing outstanding request to mess things up after a clearcache" in {
    withServices { implicit cachingService =>
      delegateService =>
        timeService =>
          val request1 = DelegateRequest("sameKey", Failure(new RuntimeException))
          val request2 = DelegateRequest("sameKey", Success("result2"))
          val request3 = DelegateRequest("sameKey", Success("result3"))

          val future1 = cachingService(request1)
          cachingService.clearCache
          checkMetrics("after future 1 started ", requests = 1, inTransitRequests = 1, inTransitFailures = 0, cacheSize = Some(0))
          request1.countDownLatch.countDown()
          checkMetrics("after future 1 finished ", requests = 1, inTransitRequests = 1, inTransitFailures = 1, cacheSize = Some(0))

          val future2 = cachingService(request2)
          request2.countDownLatch.countDown()
          checkMetrics("after future 2 started ", requests = 2, inTransitRequests = 2, inTransitFailures = 1, inTransitSucesses = 1, cacheSize = Some(1))
          Await.result(future2, 5 seconds) shouldBe "result2"
    }
  }

  it should "have a time to live, and after that time any queries blow away the cache and start again" in {
    withServices { implicit cachingService =>
      delegateService =>
        timeService =>
          val request1 = DelegateRequest("sameKey", Success("result1"))
          val request2 = DelegateRequest("sameKey", Success("result2"))

          val future1 = cachingService(request1)

          checkMetrics("after future 1 started ", requests = 1, inTransitRequests = 1, cacheSize = Some(1))
          request1.countDownLatch.countDown()
          checkMetrics("after future 1 finished ", requests = 1, inTransitRequests = 1, inTransitSucesses = 1, cacheSize = Some(1))

          when(timeService.apply()) thenReturn 1200

          val future2 = cachingService(request2)

          checkMetrics("after future 2 started ", requests = 2, inTransitRequests = 2, inTransitSucesses = 1, deadRequests = 1, cacheSize = Some(1))

          future2.isCompleted shouldBe false
          request2.countDownLatch.countDown()
          checkMetrics("after future 2 finished ", requests = 2, inTransitRequests = 2, inTransitSucesses = 2, deadRequests = 1, cacheSize = Some(1))
          Await.result(future2, 5 seconds) shouldBe "result2"
    }
  }

  it should "have a maximum size and randomly delete items if the maximum size is exceeded" in {
    withServices { implicit cachingService =>
      delegateService =>
        timeService =>
          val request1 = DelegateRequest("sameKey", Success("result1"))
          val request2 = DelegateRequest("sameKey", Success("result2"))

          val future1 = cachingService(request1)

          checkMetrics("after future 1 started ", requests = 1, inTransitRequests = 1, cacheSize = Some(1))
          request1.countDownLatch.countDown()
          checkMetrics("after future 1 finished ", requests = 1, inTransitRequests = 1, inTransitSucesses = 1, cacheSize = Some(1))

          when(timeService.apply()) thenReturn 1200

          val future2 = cachingService(request2)

          checkMetrics("after future 2 started ", requests = 2, inTransitRequests = 2, inTransitSucesses = 1, deadRequests = 1, cacheSize = Some(1))

          future2.isCompleted shouldBe false
          request2.countDownLatch.countDown()
          checkMetrics("after future 2 finished ", requests = 2, inTransitRequests = 2, inTransitSucesses = 2, deadRequests = 1, cacheSize = Some(1))
          Await.result(future2, 5 seconds) shouldBe "result2"
    }

  }

  "Caching config withNameAndSize" should "return " in {
    import ExecutionContext.Implicits._
    val config = CachingConfig[Future](SystemClockNanoTimeService, new DurationStaleCacheStategy(100, 1000))
    config.withNameAndSize("someName", 124) shouldBe CachingConfigWithNameAndSize(config, "someName", MaxMapSizeStrategy(124, 12))
    config.withNameAndSize("someName", 3) shouldBe CachingConfigWithNameAndSize(config, "someName", MaxMapSizeStrategy(3, 1))
  }

  "A  max cache size strategy" should "return the cache as is if the size of the cache is less than the max" in {
    withServices(cacheSizeStrategy = new MaxMapSizeStrategy(3, 2), fn = { implicit cachingService =>
      delegateService =>
        timeService =>
          val request1 = DelegateRequest("key1", Success("result1"))
          val request2 = DelegateRequest("key2", Success("result2"))
          val request3 = DelegateRequest("key3", Success("result3"))
          val request4 = DelegateRequest("key4", Success("result4"))
          val request5 = DelegateRequest("key5", Success("result4"))
          val request6 = DelegateRequest("key6", Success("result4"))
          val requests = Set(request1, request2, request3, request4, request5, request6)

          try {
            cachingService(request1)
            checkMetrics("after request1", requests = 1, inTransitRequests = 1, cacheSize = Some(1))
            cachingService(request2)
            checkMetrics("after request2", requests = 2, inTransitRequests = 2, cacheSize = Some(2))
            cachingService(request3)
            checkMetrics("after request3", requests = 3, inTransitRequests = 3, cacheSize = Some(3))
            val future4 = cachingService(request4)
            cachingService(request4) shouldBe future4 // it's important future4 is in the cache
            checkMetrics("after request4", requests = 5, inTransitRequests = 4, cacheSize = Some(2), removedBecauseTooFull = 2)

            val future5 = cachingService(request5)
            cachingService(request5) shouldBe future5 // it's important future5 is in the cache
            checkMetrics("after request5", requests = 7, inTransitRequests = 5, cacheSize = Some(3), removedBecauseTooFull = 2)

            val future6 = cachingService(request6)
            cachingService(request6) shouldBe future6 // it's important future6 is in the cache
            checkMetrics("after request6", requests = 9, inTransitRequests = 6, cacheSize = Some(2), removedBecauseTooFull = 4)

            cachingService.cachingMetrics.cacheSize shouldBe 2

          } finally {
            requests.foreach(_.countDownLatch.countDown()) //just clearing up threads}
          }
    })
  }
}