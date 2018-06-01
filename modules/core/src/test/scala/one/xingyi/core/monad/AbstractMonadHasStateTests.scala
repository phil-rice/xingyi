package one.xingyi.core.monad
import one.xingyi.core.FunctionFixture
import one.xingyi.core.language.{AnyLanguage, MonadLanguage}

import scala.language.higherKinds

trait AbstractMonadHasStateTests[M[_]] extends ContainerSpec[M] with AnyLanguage with MonadLanguage with FunctionFixture {

  implicit def monadWithState: MonadWithState[M]

  behavior of s"MonadWithState for ${monadWithState.getClass.getSimpleName}"

  val lv1 = LocalVariable[Int]()
  val lv2 = LocalVariable[Int]()

  it should "have a mapWith that works when LocalVariable has not been set" in {
    1.liftM[M].mapWith(lv1)(fn2(1, Seq[Int](), 2)) |> getT shouldBe 2
  }

  it should "have a mapWith that works when the LocalVariable has been set" in {
    println("start")
    val x = 1.liftResultAndPut(lv1, 10).map{x => println(s"liftResultAndPut done$x"); x}.mapWith(lv1)(fn2(1, Seq(10), 2)) |> getT shouldBe 2
//    1.liftResultAndPut(lv1, 10).mapWith(lv2)(fn2(1, Seq[Int](), 2)) |> getT shouldBe 2
  }

  it should "have a putIntoMethod" in {
    println("start2")
    1.liftM.putInto(lv1, 10).mapWith(lv1)(fn2(1, Seq(10), 2)) |> getT shouldBe 2
    1.liftM.putInto(lv1, 10).putInto(lv1, 20).mapWith(lv1)(fn2(1, Seq(10, 20), 2)) |> getT shouldBe 2
    1.liftResultAndPut(lv1, 10).putInto(lv1, 20).mapWith(lv2)(fn2(1, Seq[Int](), 2)) |> getT shouldBe 2
    1.liftResultAndPut(lv1, 10).putInto(lv2, 20).mapWith(lv1)(fn2(1, Seq(10), 2)) |> getT shouldBe 2
  }

  it should "have a mapState method " in {
    1.liftResultAndPut(lv1, 10).mapState(lv1)(fn(Seq(10), 2)) |> getT shouldBe 2
    1.liftResultAndPut(lv1, 10).mapState(lv1)(fn(Seq(10), 2)).mapWith(lv1)(fn2(2, Seq(10), 3)) |> getT shouldBe 3
  }

  it should "keep the state even with map methods " in {
    1.liftResultAndPut(lv1, 10).map(_ * 2).mapWith(lv1)(fn2(2, Seq(10), 4)) |> getT shouldBe 4
  }
  it should "keep the state even with flatMap methods " in {
    1.liftResultAndPut(lv1, 10).flatMap(fn(1, 2.liftM)).mapWith(lv1)(fn2(2, Seq(10), 4)) |> getT shouldBe 4
  }

}



