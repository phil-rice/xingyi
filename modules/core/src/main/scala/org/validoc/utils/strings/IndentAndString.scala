package org.validoc.utils.strings

import org.validoc.utils.functions.{Monoid, SemiGroup}
import org.validoc.utils.service.html.ToHtml

case class IndentAndString(indent: Int, lines: List[(Int, String)]) {
  def addLineAndIndent(line: String) = IndentAndString(indent + 1, lines :+ (indent, line))
  def insertLineAndIndent(line: String) = IndentAndString(indent + 1, (indent, line) :: lines)

  def unindent = IndentAndString(indent - 1, lines)

  override def toString: String = lines.map { case (i, s) => List.fill(i)(" ").mkString("") + s }.mkString("\n")
}

object IndentAndString {

  def merge(title: String, indentAndStrings: IndentAndString*): IndentAndString = {
    val depth = indentAndStrings.map(_.indent).max
    IndentAndString(depth+1, (depth, title) :: indentAndStrings.flatMap(_.lines).toList)
  }

  implicit object ToHtmlForIndentAndString extends ToHtml[IndentAndString] {
    override def apply(v1: IndentAndString): String = {
      s"<ul>${v1.lines.map { case (depth, s) => List.fill(depth)("&nbsp;&nbsp;").mkString("") + s }.map(s => s"<li>$s</li>").mkString("\n")}</ul>"
    }
  }

  implicit object MonoidForIndentAndString extends Monoid[IndentAndString] {
    override def add(one: IndentAndString, two: IndentAndString): IndentAndString = {
      val maxIndent = Math.max(one.indent, two.indent)
      IndentAndString(maxIndent + 1, one.lines ++ two.lines)
    }

    override def zero: IndentAndString = IndentAndString(0, List())
  }

}