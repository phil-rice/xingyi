package one.xingyi.kwikServer

import java.io.File

import one.xingyi.core.strings.Files
import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers}

class CommandExecutorTest extends FlatSpec with Matchers {

  behavior of classOf[CommandExecutor].getSimpleName

  it should "exec and capture the output stream as result" in {
    val pwd = new File(".").getAbsoluteFile
    implicitly[CommandExecutor].apply(pwd, "echo hello world", List()) shouldBe("hello world", 0)
    implicitly[CommandExecutor].apply(pwd, "echo hello\necho world", List()) shouldBe("hello\nworld", 0)
  }

}

class MavenDepsTest extends FlatSpec with Matchers with PomBundleFixture with BeforeAndAfterAll {

  behavior of classOf[MavenDeps].getSimpleName

  val mainDir = new File("temp")
  val poms = new File(mainDir, "poms")
  val m2 = new File(mainDir, "m2")

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    PomBundleToFiles(poms)(pomBundle)
  }

  it should "find the maven deps" in {
    val junit = new File(m2, "junit")
    if (junit.exists())
      Files.deleteDirectory(junit.getPath)
    junit.exists shouldBe false
    val (mavenOutput, mavenCode) = MavenDeps.findDeps(poms, "../m2", pomBundle)
    Files.printToFile(new File(poms, "log.log"))(_.print(s"Exitcode: $mavenCode\n$mavenOutput"))
    mavenCode shouldBe 0
    junit.exists shouldBe true
  }

  it should "makeTheTar" in {
    MavenDeps.andMakeItATar(mainDir, "m2", pomBundle.hash + ".tar")
  }

  it should "find the poms" in {
    MavenDeps.findPoms("temp/m2").mkString("\n") shouldBe fromResource("expectedPoms.txt")
  }
}
