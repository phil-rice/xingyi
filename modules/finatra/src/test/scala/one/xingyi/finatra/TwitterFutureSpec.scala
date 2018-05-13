package one.xingyi.finatra

import com.twitter.util.{Future => TFuture}
import one.xingyi.finatra.FinatraImplicits.ImplicitsForTest._
import one.xingyi.finatra.FinatraImplicits._
import org.scalatest.FlatSpecLike
import one.xingyi.core.functions._
import one.xingyi.core.http.Failer.failerForThrowable
import one.xingyi.core.http.ResponseCategoriserSpec
import one.xingyi.core.local.AbstractLocalOpsSpec
import one.xingyi.core.monad.{AbstractAsyncTests, AbstractMonadCanFailWithFailWithExceptionAsThrowableTests}
import one.xingyi.core.objectify.AbstractObjectifySpec

class TwitterFutureSpec extends AbstractMonadCanFailWithFailWithExceptionAsThrowableTests[TFuture] with FlatSpecLike with AbstractAsyncTests[TFuture] {
  override def async = asyncForTwitter
  override def monad = asyncForTwitter
}

class TwitterFutureAsyncPimperTests extends AbstractAsyncPimperTests[TFuture]

class FutureObjectifySpec extends AbstractObjectifySpec[TFuture, Throwable]

class TwitterFutureResponseCategoriserSpec extends ResponseCategoriserSpec[TFuture, Throwable]

class TwitterFutureLocalOpsSpec extends AbstractLocalOpsSpec[TFuture]("scala future")