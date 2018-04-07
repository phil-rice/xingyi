package one.xingyi.utils.caching

import java.util.concurrent.atomic.AtomicInteger

import one.xingyi.utils.UtilsSpec
import one.xingyi.utils.cache.{Cache, ShouldUseCache}

import scala.concurrent.Future
import one.xingyi.utils.functions.AsyncForScalaFuture._
import ImplicitsForTest._
import one.xingyi.utils.language.Language._

class CacheSpec extends UtilsSpec {

  behavior of "Cache object"

  it should "use the cached value unless should use cache is false " in {
    val cache = new Cache[Future, String, String] {
      override def raw = {v1 => Future.successful(v1 + "/raw/" + int.incrementAndGet())}
      val int = new AtomicInteger()
      override def clear() {}
      override def apply(v1: String) =Future.successful(v1 + "/apply/" + int.incrementAndGet())
    }
    //default is 'should cache'
    //    val fn =  _
    (Cache[Future, String, String](cache) apply ("123")).await() shouldBe "123/apply/1"
    (Cache[Future, String, String](cache) apply ("123")).await() shouldBe "123/apply/2"
    (Cache[Future, String, String](cache) apply ("123")).await() shouldBe "123/apply/3"

    implicit object ShouldUseCacheForStringTest extends ShouldUseCache[String] {
      override def apply(v1: String) = false
    }
    (Cache[Future, String, String](cache) apply ("123")).await() shouldBe "123/raw/4"
    (Cache[Future, String, String](cache) apply ("123")).await() shouldBe "123/raw/5"
    (Cache[Future, String, String](cache) apply ("123")).await() shouldBe "123/raw/6"

  }

}
