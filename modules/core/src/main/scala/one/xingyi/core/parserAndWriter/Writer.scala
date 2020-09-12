package one.xingyi.core.parserAndWriter

import java.text.SimpleDateFormat
import java.util.Date

trait Writer[T] {def apply(t: T): String}

object Writer {
  implicit object WriterForString extends Writer[String] {def apply(s: String): String = s}
  implicit object WriterForDouble extends Writer[Double] {def apply(s: Double): String = s.toString}
  implicit object WriterForBoolean extends Writer[Boolean] {def apply(s: Boolean) = s.toString}
}
