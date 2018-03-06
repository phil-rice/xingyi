package org.validoc.utils.functions

import org.scalatest.FlatSpecLike
import org.validoc.utils.functions.AsyncForScalaFuture.ImplicitsForTest._
import org.validoc.utils.functions.AsyncForScalaFuture._

import scala.concurrent.Future

class FutureMonadCanFailWithExceptionAndAsyncTest extends AbstractMonadCanFailWithFailWithExceptionAsThrowableTests[Future] with FlatSpecLike with AbstractAsyncTests[Future] {
  override def async = AsyncForScalaFuture.defaultAsyncForScalaFuture
  override def monad = AsyncForScalaFuture.defaultAsyncForScalaFuture
}
