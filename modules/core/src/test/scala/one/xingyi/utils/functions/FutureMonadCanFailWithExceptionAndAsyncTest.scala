package one.xingyi.utils.functions

import org.scalatest.FlatSpecLike
import one.xingyi.utils.functions.AsyncForScalaFuture.ImplicitsForTest._
import one.xingyi.utils.functions.AsyncForScalaFuture._

import scala.concurrent.Future

class FutureMonadCanFailWithExceptionAndAsyncTest extends AbstractMonadCanFailWithFailWithExceptionAsThrowableTests[Future] with FlatSpecLike with AbstractAsyncTests[Future] {
  override def async = AsyncForScalaFuture.defaultAsyncForScalaFuture
  override def monad = AsyncForScalaFuture.defaultAsyncForScalaFuture
}
