package one.xingyi.core.functions

import org.scalatest.FlatSpecLike
import one.xingyi.core.functions.AsyncForScalaFuture.ImplicitsForTest._
import one.xingyi.core.functions.AsyncForScalaFuture._

import scala.concurrent.Future

class FutureMonadCanFailWithExceptionAndAsyncTest extends AbstractMonadCanFailWithFailWithExceptionAsThrowableTests[Future] with FlatSpecLike with AbstractAsyncTests[Future] {
  override def async = AsyncForScalaFuture.defaultAsyncForScalaFuture
  override def monad = AsyncForScalaFuture.defaultAsyncForScalaFuture
}
