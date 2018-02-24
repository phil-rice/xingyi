package org.validoc.utils

import java.util.concurrent.Executors

import org.mockito.ArgumentCaptor
import org.scalatest.concurrent.Eventually
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{FlatSpec, Matchers}
import org.validoc.utils.concurrency.{Async, MDCPropagatingExecutionContext}
import org.validoc.utils.logging.NullLoggingAdapterWithMdc

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.{higherKinds, postfixOps}
import scala.reflect.ClassTag

class UtilsSpec extends FlatSpec with Matchers with MockitoSugar with Eventually {
  implicit lazy val ec: MDCPropagatingExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(10))

  def await[X](f: Future[X]) = Await.result(f, 5 seconds)


  def captorFor[C: ClassTag] = ArgumentCaptor.forClass(implicitly[ClassTag[C]].runtimeClass).asInstanceOf[ArgumentCaptor[C]]

  def compareSequences(a: Seq[_], b: Seq[_]) = {
    for (((a, b), i) <- a.zipAll(b, null, null).zipWithIndex) {
      withClue(s"Item $i") {
        a shouldBe b
      }
    }
  }

}

class UtilsWithLoggingSpec extends UtilsSpec {
  implicit val loggingAdapter = NullLoggingAdapterWithMdc
}

class UtilsWithExecutionContextSpec extends UtilsSpec {
  implicit val mdc: MDCPropagatingExecutionContext = ExecutionContext.global
}