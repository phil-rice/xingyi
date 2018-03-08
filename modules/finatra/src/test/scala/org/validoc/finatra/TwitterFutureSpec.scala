package org.validoc.finatra

import com.twitter.util.{Duration => TDuration, Future => TFuture, Try => TTry}
import org.scalatest.FlatSpecLike
import org.validoc.finatra.FinatraImplicits.ImplicitsForTest._
import org.validoc.finatra.FinatraImplicits._
import org.validoc.utils.functions._
import org.validoc.utils.http.ResponseCategoriserSpec
import org.validoc.utils.local.AbstractLocalOpsSpec
import org.validoc.utils.objectify.AbstractObjectifySpec

import org.validoc.utils.http.Failer.failerForThrowable

class TwitterFutureSpec extends AbstractMonadCanFailWithFailWithExceptionAsThrowableTests[TFuture] with FlatSpecLike with AbstractAsyncTests[TFuture] {
  override def async = asyncForTwitter
  override def monad = asyncForTwitter
}

class TwitterFutureAsyncPimperTests extends AbstractAsyncPimperTests[TFuture]

class FutureObjectifySpec extends AbstractObjectifySpec[TFuture, Throwable]

class TwitterFutureResponseCategoriserSpec extends ResponseCategoriserSpec[TFuture, Throwable]

class TwitterFutureLocalOpsSpec extends AbstractLocalOpsSpec[TFuture]("scala future")