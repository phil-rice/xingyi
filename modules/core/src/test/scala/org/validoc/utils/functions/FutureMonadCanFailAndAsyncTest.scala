package org.validoc.utils.functions

import org.scalatest.FlatSpecLike
import org.validoc.utils.functions.AsyncForScalaFuture.ImplicitsForTest._
import org.validoc.utils.functions.AsyncForScalaFuture._

import scala.concurrent.Future

class FutureMonadCanFailAndAsyncTest extends AbstractMonadCanFailWithFailAsThrowableTests[Future] with FlatSpecLike with AbstractAsyncTests[Future] {

  it should "test" in {

  }
  override def async = AsyncForScalaFuture.defaultAsyncForScalaFuture
  override def monad = AsyncForScalaFuture.defaultAsyncForScalaFuture
}

object thing extends App {
  org.scalatest.run.apply(new FutureMonadCanFailAndAsyncTest)
  println("done")
}