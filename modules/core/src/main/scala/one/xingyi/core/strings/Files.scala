/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.strings

import java.io.{File, FileNotFoundException, FileOutputStream, IOException}
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, FileVisitor, Path, Paths, Files => NioFiles}
import scala.annotation.tailrec


object Files {

  def findFileRecursivelyUpParents(offset: String): File = findFileRecursivelyUpParents(new File("."), offset)
  def findFileRecursivelyUpParents(dir: File, offset: String): File = findFileRecursivelyUpParents(List(dir), dir.getAbsoluteFile, offset)
  @tailrec
  private def findFileRecursivelyUpParents(dirs: List[File], dir: File, offset: String): File = {
    if (dir == null) throw new FileNotFoundException("tried\n" + dirs.map(new File(_, offset)).mkString("\n")) else {
      val file = new File(dir, offset)
      if (file.exists()) file else findFileRecursivelyUpParents(dir :: dirs, dir.getParentFile, offset)
    }
  }

  def walkFiles(directory: String)(fn: Path => Unit) = {
    NioFiles.walkFileTree(Paths.get(directory), new FileVisitor[Path] {
      def visitFileFailed(file: Path, exc: IOException): FileVisitResult = FileVisitResult.CONTINUE

      def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        fn(file);
        FileVisitResult.CONTINUE
      }

      def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult = FileVisitResult.CONTINUE

      def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = FileVisitResult.CONTINUE
    })
  }

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    try {
      f.getCanonicalFile.getParentFile.mkdirs()
    } catch {
      case e: Exception => throw new RuntimeException(s"File: $f", e)
    }
    val p = new java.io.PrintWriter(f)
    try {
      op(p)
    } finally {
      p.close()
    }

  }

  def appendToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    try {
      f.getCanonicalFile.getParentFile.mkdirs()
    } catch {
      case e: Exception => throw new RuntimeException(s"File: $f", e)
    }
    val p = new java.io.PrintWriter(new FileOutputStream(f, true))
    try {
      op(p)
    } finally {
      p.close()
    }

  }
}


