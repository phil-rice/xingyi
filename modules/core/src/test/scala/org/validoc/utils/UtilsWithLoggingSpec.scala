package org.validoc.utils

import java.util.concurrent.Executors

import org.mockito.ArgumentCaptor
import org.scalatest.concurrent.Eventually
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{FlatSpec, FlatSpecLike, Matchers}
import org.validoc.utils.local.ExecutionContextWithLocal
import org.validoc.utils.logging.NullLoggingAdapterWithMdc

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.{higherKinds, postfixOps}
import scala.reflect.ClassTag

trait UtilsSpec extends FlatSpecLike with Matchers with MockitoSugar with Eventually {

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

trait UtilsWithLoggingSpec extends UtilsSpec {
  implicit val loggingAdapter = NullLoggingAdapterWithMdc
}

//class UtilsWithExecutionContextSpec extends UtilsSpec {
//  implicit val mdc: MDCPropagatingExecutionContext = ExecutionContext.global
//}