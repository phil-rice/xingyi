package one.xingyi.finatra

import com.twitter.util.{Future => TFuture}
import one.xingyi.finatra.FinatraImplicits.ImplicitsForTest._
import one.xingyi.finatra.FinatraImplicits._
import org.scalatest.FlatSpecLike
import one.xingyi.utils.functions._
import one.xingyi.utils.http.Failer.failerForThrowable
import one.xingyi.utils.http.ResponseCategoriserSpec
import one.xingyi.utils.local.AbstractLocalOpsSpec
import one.xingyi.utils.objectify.AbstractObjectifySpec

class TwitterFutureSpec extends AbstractMonadCanFailWithFailWithExceptionAsThrowableTests[TFuture] with FlatSpecLike with AbstractAsyncTests[TFuture] {
  override def async = asyncForTwitter
  override def monad = asyncForTwitter
}

class TwitterFutureAsyncPimperTests extends AbstractAsyncPimperTests[TFuture]

class FutureObjectifySpec extends AbstractObjectifySpec[TFuture, Throwable]

class TwitterFutureResponseCategoriserSpec extends ResponseCategoriserSpec[TFuture, Throwable]

class TwitterFutureLocalOpsSpec extends AbstractLocalOpsSpec[TFuture]("scala future")