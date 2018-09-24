package one.xingyi.core.optics
import one.xingyi.core.{FunctionFixture, UtilsSpec}

trait OpticsTestThing
case class OpticsGrandchild(e: Int) extends OpticsTestThing
case class OpticsChild(c: Int, d: OpticsGrandchild) extends OpticsTestThing
case class OpticsMain(a: Int, b: OpticsChild) extends OpticsTestThing

class OpticsSpec extends UtilsSpec with FunctionFixture {

  val lensMaina = Lens[OpticsMain, Int](_.a, (m, a) => m.copy(a = a))
  val lensMainChild = Lens[OpticsMain, OpticsChild](_.b, (m, b) => m.copy(b = b))
  val lensChildc = Lens[OpticsChild, Int](_.c, (m, c) => m.copy(c = c))
  val lensChildGrandChild = Lens[OpticsChild, OpticsGrandchild](_.d, (m, d) => m.copy(d = d))
  val lensGrandChilde = Lens[OpticsGrandchild, Int](_.e, (m, e) => m.copy(e = e))
  val lensMainChildc = lensMainChild andThen lensChildc
  val lensMainChildGrandchild = lensMainChild andThen lensChildGrandChild
  val lensMainChildGrandchilde = lensMainChild andThen lensChildGrandChild andThen (lensGrandChilde)

  val main = OpticsMain(1, OpticsChild(2, OpticsGrandchild(3)))
  behavior of "Lens"

  it should "allow values to be got" in {
    lensMaina.get(main) shouldBe 1
    lensMainChild.get(main) shouldBe main.b
    lensChildc.get(main.b) shouldBe 2
    lensMainChildc.get(main) shouldBe 2
    lensMainChildGrandchild.get(main) shouldBe main.b.d
  }

  it should "allow values to be set " in {
    lensMaina.set(main, 66) shouldBe OpticsMain(66, OpticsChild(2, OpticsGrandchild(3)))
    lensMainChildGrandchilde.set(main, 6) shouldBe OpticsMain(1, OpticsChild(2, OpticsGrandchild(6)))
  }

  it should "allow values to be mapped " in {
    lensMaina.map(_ + 1)(main) shouldBe OpticsMain(2, OpticsChild(2, OpticsGrandchild(3)))
    lensMainChildGrandchilde.map(_ + 1)(main) shouldBe OpticsMain(1, OpticsChild(2, OpticsGrandchild(4)))
  }

  it should "have an 'andGet' method" in {
    lensMaina andGet (_.toString) apply main shouldBe "1"
    lensMainChildGrandchilde andGet (_.toString) apply main shouldBe "3"
  }
  //  it should "have an 'andSet' method" in {
  //    lensMaina andSet(_ * 2) apply main shouldBe "1"
  //    lensMainChildGrandchilde andGet(_.toString) apply main shouldBe "3"
  //  }

  it should "have an identity Lens" in {
    Lens.identity(main) shouldBe main
    lensMainChild andThen lensChildc andThen Lens.identity set(main, 4) shouldBe OpticsMain(1, OpticsChild(4, OpticsGrandchild(3)))
  }

  it should "have a cast Lens that just changes the type (unsafely!)" in {
    val lensMainChildT = Lens[OpticsMain, OpticsTestThing](_.b, (m, b) => m.copy(b = b.asInstanceOf[OpticsChild]))

    lensMainChildT andThen Lens.cast[OpticsTestThing, OpticsChild] set(main, OpticsChild(7, OpticsGrandchild(8))) shouldBe OpticsMain(1, OpticsChild(7, OpticsGrandchild(8)))
    lensMainChildT andThen Lens.cast[OpticsTestThing, OpticsChild] get main shouldBe OpticsChild(2, OpticsGrandchild(3))

  }

}
