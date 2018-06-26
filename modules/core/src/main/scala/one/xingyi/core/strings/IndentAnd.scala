/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.strings

import javax.swing.JToolBar.Separator

import one.xingyi.core.functions.{Monoid, SemiGroup}
import one.xingyi.core.service.html.ToHtml

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
