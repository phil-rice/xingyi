package one.xingyi.kwikServer

import java.io.File

import one.xingyi.core.strings.Files
import one.xingyi.javaserver.Streams

import scala.collection.JavaConverters
import scala.io.Source

trait MakeTempDir {
  def apply(prefix: String): File
}

object MakeTempDir {
  implicit def defaultMakeTempDir: MakeTempDir = prefix => java.nio.file.Files.createTempDirectory(prefix).toFile

  def fixedTempDir(file: File): MakeTempDir = _ => file
}

object PomBundleToFiles extends PomBundleToFiles

trait PomBundleToFiles {
  def apply(dir: File)(pomBundle: PomBundle): Unit = {
    pomBundle.pomData.foreach { pomData =>
      val file = new File(dir, pomData.relativePath) //TODO validate that the relative path is sensible (i.e. under the dir)
      file.getParentFile.mkdirs()
      Files.printToFile(file)(pw => pw.println(pomData.pom))
    }
  }

  def toTempFile(pomBundle: PomBundle)(implicit makeTempDir: MakeTempDir): File = {
    val dir = java.nio.file.Files.createTempDirectory("deps_" + pomBundle.hash).toFile
    apply(dir)(pomBundle)
    dir
  }
}

trait CommandExecutor {
  def apply(workingDir: File, command: String, envVariables: Seq[Variable]): (String, Int)
}

object CommandExecutor {
  implicit val bashCommandExecutor: CommandExecutor =
    (workingFolder, command, envVariables) => exec(workingFolder, List("bash", "-c", command), envVariables)

  def exec(workingFolder: File, commands: List[String], envVariables: Seq[Variable]): (String, Int) = {
    val pb: ProcessBuilder = new ProcessBuilder(JavaConverters.seqAsJavaList(commands))
    pb.redirectErrorStream(true)
    val env = pb.environment
    envVariables.foreach(v => env.put(v.name, v.value)) //TODO security hole: validate
    pb.directory(workingFolder)
    val proc = pb.start
    (Streams.readAll(proc.getInputStream), proc.waitFor())
  }

}

trait MavenDeps {
  def findDeps(workingDir: File, m2: String, pomBundle: PomBundle)(implicit commandExecutor: CommandExecutor): (String, Int)
}

object MavenDeps extends MavenDeps {
  def findDeps(workingDir: File, m2: String, pomBundle: PomBundle)(implicit commandExecutor: CommandExecutor): (String, Int) = {
    commandExecutor(workingDir, s"mvn dependency:list -Dmaven.repo.local=$m2", pomBundle.envVariables)
  }

  def andMakeItATar(workingDir: File, m2: String, tarName: String)(implicit commandExecutor: CommandExecutor) =
    commandExecutor(workingDir, s"tar -cvf $tarName $m2", List())

  import java.io.IOException
  import java.nio.file.attribute.BasicFileAttributes
  import java.nio.file.{FileVisitResult, FileVisitor, Path, Paths, Files => NioFiles}


  def findPoms(m2: String): List[String] = {
    var result = List[String]()
    val m2Dir = new File(m2).toPath
    Files.walkFiles(m2) { path =>
      if (path.toFile.getName.endsWith(".pom"))
        result = m2Dir.relativize(path).toFile.getPath :: result
    }
    result.reverse
  }


}

