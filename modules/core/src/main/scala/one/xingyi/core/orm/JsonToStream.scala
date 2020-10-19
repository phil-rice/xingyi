package one.xingyi.core.orm

import java.io.OutputStream

import scala.language.higherKinds

trait JsonToStreamFor[Schema[_]] {
  def putToJson[T](s: Schema[T]): JsonToStream[Schema, T]
}
trait JsonToStream[F[_], T] {
  def put(f: F[T], t: Any, stream: OutputStream)
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

  def asStringWithQuotes[F[_], T]: JsonToStream[F, T] = new JsonToStream[F, T] {
    override def put(f: F[T], s: Any, stream: OutputStream): Unit = putEscapedWithQuotes(s.toString, stream)
  }

  def asStringFnWithQuotes[F[_], T](fn: Any => String): JsonToStream[F, T] = new JsonToStream[F, T] {
    override def put(f: F[T], s: Any, stream: OutputStream): Unit = putEscapedWithQuotes(fn(s), stream)
  }
  def asStringFnWithoutQuotes[F[_], T](fn: Any => String): JsonToStream[F, T] = new JsonToStream[F, T] {
    override def put(f: F[T], s: Any, stream: OutputStream): Unit = putUnescaped(stream, fn(s))
  }


  implicit def StringJsonToStream[F[_]]: JsonToStream[F, String] = asStringWithQuotes //(s: Any, stream: OutputStream) => putEscapedWithQuotes(s.toString, stream)
  implicit def IntJsonToStream[F[_]]: JsonToStream[F, Int] = asStringFnWithoutQuotes(_.toString)
  implicit def DoubleJsonToStream[F[_]]: JsonToStream[F, Double] = asStringFnWithoutQuotes(_.toString)
  implicit def PlaceholderJsonToStream[F[_]]: JsonToStream[F, Placeholder] = new JsonToStream[F, Placeholder] {
    override def put(f: F[Placeholder], t: Any, stream: OutputStream): Unit = throw new RuntimeException("Should not attempt to write a placeholder to the output stream")
  }
}