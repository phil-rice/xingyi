package one.xingyi.core.map

import java.io.{File, PrintWriter}

trait FileToPrintWriter {
  def apply(f: File): PrintWriter
}
object FileToPrintWriter {
  implicit object defaultFileToPrintWriter extends FileToPrintWriter {
    override def apply(f: File): PrintWriter = new PrintWriter(f)
  }
}

object PrintWriterMap {
  def apply[T](fileFn: T => File)(implicit fileToPrintWriter: FileToPrintWriter): PrintWriterMap[T] = new PrintWriterMapImpl[T](fileFn)
}

trait PrintWriterMap[T] {
  /** t is used to calculate a file, and s is appended to the end of the file.
   * The first time a file is used, it is opened. It stays open until the close method is called */
  def writeln(t: T, s: String)
  def close()
}

class PrintWriterMapImpl[T](fileFn: T => File)(implicit fileToPrintWriter: FileToPrintWriter) extends PrintWriterMap[T] {
  var stillOpen = true
  val fileMap = new SingletonMap[T, PrintWriter](t => fileToPrintWriter(fileFn(t)))
  def writeln(t: T, s: String) {require(stillOpen, "Called writeln after closed"); fileMap(t).println(s) }
  def close() {stillOpen = false; fileMap.map.values.foreach(_.close()) }
}
