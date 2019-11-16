/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.strings

import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, FileVisitor, Path, Paths, Files => NioFiles}


object Files {

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
      println(f)
      f.getParentFile.mkdirs()
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


  @throws[IOException]
  def deleteDirectory(directoryFilePath: String): Unit = {
    import java.io.IOException
    import java.nio.file.FileVisitResult
    import java.nio.file.Files
    import java.nio.file.Paths
    import java.nio.file.SimpleFileVisitor
    import java.nio.file.attribute.BasicFileAttributes
    val directory = Paths.get(directoryFilePath)
    if (NioFiles.exists(directory)) NioFiles.walkFileTree(directory, new SimpleFileVisitor[Path]() {
      override def visitFile(path: Path, basicFileAttributes: BasicFileAttributes): FileVisitResult = {
        NioFiles.delete(path)
        FileVisitResult.CONTINUE
      }

      override def postVisitDirectory(directory: Path, ioException: IOException): FileVisitResult = {
        NioFiles.delete(directory)
        FileVisitResult.CONTINUE
      }
    })
  }
}


