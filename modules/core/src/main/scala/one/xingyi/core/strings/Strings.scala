/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.strings

import java.io.{ByteArrayOutputStream, PrintStream, StringWriter}

import javax.swing.JPopupMenu.Separator

class ParseException(msg: String, e: Exception) extends RuntimeException(msg, e) {
  def this(msg: String) = this(msg, null)
}
object Strings {
  def split(separator: String = "\\."): (String => List[String]) = s => s.split(separator).filter(_.nonEmpty).map(_.trim).toList
  def splitInTwoException(separator: String, s: String) = throw new ParseException(s"Cannot split a string into two non empty parts using [$separator] string was [$s]")
  def splitInTwo(separator: String = ":", defaultRhs: (String, String) => String = splitInTwoException): (String => (String, String)) = { s =>
    split(separator)(s) match {
      case List(left) => (left, defaultRhs(separator, s))
      case List(left, right) => (left, right)
      case _ => throw new ParseException(s"Cannot split a string into two non empty parts using [$separator] string was [$s]")
    }
  }
  def withoutStringBefore(beforeChar: Char)(s: String): String = s.dropWhile(_ != beforeChar).drop(1)

  def toOption(s: String) = if (s == null || s == "") None else Some(s)

  def startsWithAndSnips(start: String)(s: String): Option[String] = if (s.startsWith(start)) Some(s.substring(start.length + 1)) else None

  def removeOptional$(s: String) = s.reverse.dropWhile(_ == '$').reverse

  def uppercaseFirst(s: String) = s.take(1).map(_.toUpper) ++ s.drop(1)

  def lowercaseFirst(s: String) = s.take(1).map(_.toLower) ++ s.drop(1)

  def useStringWriter(fn: StringWriter => Unit) = {
    val writer = new StringWriter()
    fn(writer)
    writer.flush()
    writer.toString
  }

  def classNameOfObject(obj: Object): String = obj.getClass.getSimpleName.dropRight(1)

  def indent(filler: String, depth: Int): String = List.fill(depth)(filler).mkString("")

  def indentTuple(filler: String, left: Int, mid: Int)(tuple: (String, String)) = {
    val padding = mid - left - tuple._1.length
    indent(filler, left) + tuple._1 + indent(filler, padding) + tuple._2
  }

  //This can obviously be optimised and actually should be I think...
  //Using a library is possible but painful and it's a lot of pulled in code just to escape a string
  def escapeJson(raw: String) = {
    var escaped = raw
    escaped = escaped.replace("\\", "\\\\")
    escaped = escaped.replace("\"", "\\\"")
    escaped = escaped.replace("\b", "\\b")
    escaped = escaped.replace("\f", "\\f")
    escaped = escaped.replace("\n", "\\n")
    escaped = escaped.replace("\r", "\\r")
    escaped = escaped.replace("\t", "\\t")
    // TODO: escape other non-printing characters using uXXXX notation
    escaped
  }

  def paramsToMap(s: String): Map[String, String] = {
    s.split("&").map {
      _.split("=") match {
        case Array(name, value) => (name, value)
        case x => throw new RuntimeException(s"Do not understand [$x] as part of $s")
      }
    }.toMap
  }

  def removeWhiteSpace(s: String): String = s.replaceAll("\\s+", "")

  def ellipses(maxLength: Int)(s: String): String = if (s.length > maxLength) s.take(maxLength) + ".." else s

  def lastSection(marker: String)(s: String) = s.split(marker).last

  def lastButOneSection(marker: String)(s: String) = {
    val split = s.split(marker)
    val splitDroppedRight = split.dropRight(1)
    val result = splitDroppedRight.last
    result
  }

  def allButlastSection(marker: String)(s: String) = s.split(marker).dropRight(1).mkString(marker)

  def recordPrintln[X](x: => X): (X, String) = {
    val bytes = new ByteArrayOutputStream()
    val result = Console.withOut(new PrintStream(bytes))(x)
    (result, bytes.toString("UTF-8"))
  }

  def trimChar(trim: Char)(s: String) = s.dropWhile(_ == trim).reverse.dropWhile(_ == trim).reverse


  def cleanString(s: String, acceptedChars: String = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_- ") = s.filter(acceptedChars.contains(_)).mkString

  def uri(parts: String*): String = parts.map(trimChar('/')).mkString("/")
  def extractFromUrl(template: String): String => List[String] = new StringsUrlExtractor(template)

}
class StringsUrlExtractor(template: String) extends (String => List[String]) {
  val templateParts = Strings.split("/")(template)
  val (data, filler) = templateParts.zipWithIndex.partition(s => s._1.startsWith("{") && s._1.endsWith("}"))
  require(filler.forall(f => !(f._1.contains("{") || f._1.contains("}"))), s"Cannot have { or } in the template [$template] except in format /{n}/.")
  val dataNumbers = try {
    data.map { case (s, i) => (s.drop(1).dropRight(1).toInt, i) }.sortBy(_._1)
  } catch {case e: Exception => throw new RuntimeException(s"Error finding the numbers in template [$template]", e)}
  require(dataNumbers.map(_._1) == (0 to dataNumbers.size - 1), s"Must have consecutive {0}/{1} etc in template: [$template] dataNumbers ${dataNumbers}")
  val indexes = dataNumbers.map(_._2)

  def apply(data: String): List[String] = {
    val parts: List[String] = Strings.split("/")(data)
    val matches = templateParts.size == parts.size && filler.forall { case (s, i) => s == parts(i) }
    if (matches) indexes.map(parts(_)) else Nil
  }
}

