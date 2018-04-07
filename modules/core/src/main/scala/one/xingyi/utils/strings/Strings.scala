package one.xingyi.utils.strings

import java.io.{ByteArrayOutputStream, PrintStream}

import one.xingyi.utils.metrics.PrintlnPutMetrics

object Strings {
  def classNameOfObject(obj: Object): String = obj.getClass.getSimpleName.dropRight(1)
  def indent(filler: String, depth: Int): String = List.fill(depth)(filler).mkString("")

  def indentTuple(filler: String, left: Int, mid: Int)(tuple: (String, String)) = {
    val padding = mid - left - tuple._1.length
    indent(filler, left) + tuple._1 + indent(filler, padding) + tuple._2
  }


  def removeWhiteSpace(s: String): String = s.replaceAll("\\s+", "")

  def ellipses(maxLength: Int)(s: String): String = if (s.length > maxLength) s.take(maxLength) + ".." else s

  def lastSection(marker: String)(s: String) = s.split(marker).last
  def allButlastSection(marker: String)(s: String) = s.split(marker).dropRight(1).mkString(marker)
  def recordPrintln[X](x: => X): (X, String) = {
    val bytes = new ByteArrayOutputStream()
    val result = Console.withOut(new PrintStream(bytes))(x)
    (result, bytes.toString("UTF-8"))
  }
}

