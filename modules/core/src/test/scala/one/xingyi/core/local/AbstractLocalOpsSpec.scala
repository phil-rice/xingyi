package one.xingyi.core.local

import one.xingyi.core.language.Language._
import one.xingyi.core.monad.{Async, Monad}
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.Future
import scala.language.higherKinds
import scala.reflect.ClassTag

abstract class AbstractLocalOpsSpec[M[_] : Monad : Async](name: String)(implicit localOps: LocalOps[M]) extends FlatSpec with Matchers with LocalOpsPimper[M] {

  behavior of s"LocalOps for $name"

  def goThroughMapAndFlatMap[T: ClassTag]() = 1.liftM[M].flatMap { x: Int => x.liftM[M] }.map { x: Int => getFromLocalStore[Int] }.await


  it should "allow a local to be created and values put and get in it" in {
    clearlocalStore[Int]()
    getFromLocalStore[Int]() shouldBe None
    putInlocalStore(1)

    getFromLocalStore[Int]() shouldBe Some(1)
    putInlocalStore(2)

    getFromLocalStore[Int]() shouldBe Some(2)
    clearlocalStore[Int]()
    getFromLocalStore[Int]() shouldBe None

  }


  it should "allow a local to be created and the values available after maps and flatmaps" in {
    clearlocalStore()

    goThroughMapAndFlatMap() shouldBe None
    putInlocalStore(1)
    goThroughMapAndFlatMap() shouldBe Some(1)
    putInlocalStore(2)
    goThroughMapAndFlatMap() shouldBe Some(2)
    clearlocalStore[Int]()
    goThroughMapAndFlatMap() shouldBe None
  }
}

import one.xingyi.core.monad.AsyncForScalaFuture.ImplicitsForTest._
import one.xingyi.core.monad.AsyncForScalaFuture._

class ScalaFutureLocallOpsSpec extends AbstractLocalOpsSpec[Future]("scala future")
