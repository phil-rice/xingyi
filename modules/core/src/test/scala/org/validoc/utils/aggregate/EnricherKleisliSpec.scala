package org.validoc.utils.aggregate

import org.validoc.utils.{AsyncFixture, UtilsSpec}
import org.validoc.utils.functions.{Async, Monad, MonadCanFailWithException}

import scala.util.Success
import org.validoc.utils.language.Language._

import scala.concurrent.Future
import scala.language.higherKinds

class EnricherKleisliSpec[M[_], Fail](implicit monadCanFail: MonadCanFailWithException[M, Fail], async: Async[M]) extends UtilsSpec with AsyncFixture[M] {

  type Kleisli[Req, Res] = Req => M[Res]
  val enricher = new EnrichLanguage[Kleisli] with EnrichKleisli[M] {
    override protected implicit def monad: Monad[M] = monadCanFail
  }

  import enricher._

  behavior of "Enricher"

  it should "allow a kleisli to enrich another" in {
    val getIds = kleisli(1, Success("1,2,3"))
    def getValue(id: String) = s"value($id)".liftM
    implicit object hasChildrenForString extends HasChildren[String, String] {
      override def apply(v1: String): Seq[String] = v1.split(",")
    }
    implicit object enricher extends Enricher[Int, String, String, String, String] {
      override def apply(parentReq: Int, parent: String, childIdsAndValues: Seq[(String, String)]): String =
        s"$parentReq/$parent/[${childIdsAndValues.mkString(",")}]"
    }
    val x: Kleisli[Int, String] = enrich[Int, String](getIds).withChild(getValue).mergeInto[String]
    x(1).await shouldBe "1/1,2,3/[(1,value(1)),(2,value(2)),(3,value(3))]"
  }
}
import org.validoc.utils.functions.AsyncForScalaFuture._
import ImplicitsForTest._

class ScalaFutureEnricherKleisliSpec extends EnricherKleisliSpec[Future, Throwable]