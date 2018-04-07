package one.xingyi.utils.strings

import javax.swing.JToolBar.Separator

import one.xingyi.utils.functions.{Monoid, SemiGroup}
import one.xingyi.utils.service.html.ToHtml

case class IndentAnd[T](indent: Int, lines: List[(Int, T)]) {
  def addLineAndIndent(line: T) = IndentAnd(indent + 1, lines :+ (indent, line))
  def insertLineAndIndent(line: T) = IndentAnd(indent + 1, (indent, line) :: lines)

  def unindent = IndentAnd(indent - 1, lines)

  val maxIndent: Int = if (lines.isEmpty) 0 else lines.map(_._1).max
  def invertIndent = {
    IndentAnd(indent, lines.map { case (i, s) => (maxIndent - i, s) })
  }
  def offset(by: Int) = IndentAnd(indent, lines.map { case (i, s) => (i + by, s) })

  def defaultToString(separator: String, filler: String) = toString(separator, IndentAnd.defaultToString(filler, _.toString))

  def toString(separator: String, fn: (Int, T) => String): String = lines.map { case (i, t) => fn(i, t) }.mkString(separator)
}

object IndentAnd {

  def tupleToString(filler: String, mid: Int)(depth: Int, tuple: (String, String)) =
    Strings.indentTuple(filler, depth, mid)(tuple)


  def defaultToString[T](filler: String, fn: T => String)(depth: Int, t: T) = Strings.indent(filler, depth) + fn(t)


  def maxOf[T](indentAndString: Seq[IndentAnd[T]], fn: IndentAnd[T] => Int) =
    if (indentAndString.size == 0) 0 else indentAndString.map(fn).max

  def merge[T](title: T, indentAndStrings: IndentAnd[T]*): IndentAnd[T] = {
    val depth = maxOf[T](indentAndStrings, _.indent)
    val maxIndent = maxOf[T](indentAndStrings, _.maxIndent)
    val normalised = indentAndStrings.map { case i@IndentAnd(indent, lines) => i.offset(maxIndent - i.maxIndent) }
    IndentAnd[T](depth + 1, (depth, title) :: normalised.flatMap(_.lines).toList)
  }

  //  implicit object ToHtmlForIndentAndString extends ToHtml[IndentAndString] {
  //    override def apply(v1: IndentAndString): String = {
  //      s"<ul>${v1.lines.map { case (depth, s) => List.fill(depth)("&nbsp;&nbsp;").mkString("") + s }.map(s => s"<li>$s</li>").mkString("\n")}</ul>"
  //    }
  //  }


}