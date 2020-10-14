package one.xingyi.core.orm

import java.io.OutputStream

import scala.language.higherKinds

trait JsonToStreamFor[Context, Schema[_]] {
  def putToJson[T](c: Context, s: Schema[T]): JsonToStream[Context, Schema, T]
}
trait JsonToStream[Context, F[_], T] {
  def put(c: Context, f: F[T], t: Any, stream: OutputStream)
}

object JsonToStream {
  def putUnescaped(outputStream: OutputStream, s: String) {outputStream.write(s.getBytes) }
  def putEscaped(s: String, stream: OutputStream) {
    var i = 0
    def escape(c: Char): Unit = {stream.write('\\'); stream.write(c) }
    while (i < s.length) {
      s.charAt(i) match {
        case ('\\') => escape('\\')
        case ('\b') => escape('b')
        case ('\f') => escape('f')
        case ('\n') => escape('n')
        case ('\r') => escape('r')
        case ('\t') => escape('t')
        case ('"') => escape('"')
        case c => stream.write(c)
      }
      i += 1
    }
  }
  def putEscapedWithQuotes(s: String, stream: OutputStream) {
    stream.write('"')
    putEscaped(s, stream)
    stream.write('"')
  }

  def asStringWithQuotes[Context, F[_], T]: JsonToStream[Context, F, T] = (c: Context, f: F[T], s: Any, stream: OutputStream) => putEscapedWithQuotes(s.toString, stream)
  def asStringFnWithQuotes[Context, F[_], T](fn: Any => String): JsonToStream[Context, F, T] = (c: Context, f: F[T], s: Any, stream: OutputStream) => putEscapedWithQuotes(fn(s), stream)

  implicit def StringJsonToStream[Context, F[_]]: JsonToStream[Context, F, String] = asStringWithQuotes //(s: Any, stream: OutputStream) => putEscapedWithQuotes(s.toString, stream)
  implicit def IntJsonToStream[Context, F[_]]: JsonToStream[Context, F, Int] = (c: Context, f: F[Int], t: Any, stream: OutputStream) => putUnescaped(stream, t.toString)
  implicit def DoubleJsonToStream[Context, F[_]]: JsonToStream[Context, F, Double] = (c: Context, f: F[Double], t: Any, stream: OutputStream) => putUnescaped(stream, t.toString)
  implicit def PlaceholderJsonToStream[Context, F[_]]: JsonToStream[Context, F, Placeholder] = (c: Context, f: F[Placeholder], t: Any, stream: OutputStream) => throw new RuntimeException("Should not attempt to write a placeholder to the output stream")
}