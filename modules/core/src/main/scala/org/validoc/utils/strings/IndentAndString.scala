package org.validoc.utils.strings

import org.validoc.utils.functions.{Monoid, SemiGroup}

case class IndentAndString(indent: Int, lines: List[(Int, String)]) {
  def addLineAndIndent(line: String) = IndentAndString(indent + 1, lines :+ (indent, line))

  def unindent = IndentAndString(indent - 1, lines)

  override def toString: String = lines.map { case (i, s) => List.fill(i)(" ").mkString("") + s }.mkString("\n")
}

object IndentAndString {

  implicit object MonoidForIndentAndString extends Monoid[IndentAndString] {
    override def add(one: IndentAndString, two: IndentAndString): IndentAndString = {
      val maxIndent = Math.max(one.indent, two.indent)
      IndentAndString(maxIndent + 1, one.lines ::: two.lines)
    }

    override def zero: IndentAndString = IndentAndString(0, List())
  }

}