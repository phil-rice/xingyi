package org.validoc.utils.strings

case class IndentAndString(indent: Int, lines: List[(Int, String)]) {
  def addLineAndIndent(line: String) = IndentAndString(indent + 1, lines :+ (indent, line))

  def unindent = IndentAndString(indent - 1, lines)

  override def toString: String = lines.map { case (i, s) => List.fill(i)(" ").mkString("") + s }.mkString("\n")
}