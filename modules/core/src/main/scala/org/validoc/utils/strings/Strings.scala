package org.validoc.utils.strings

object Strings {
  def classNameOfObject(obj: Object) = obj.getClass.getSimpleName.dropRight(1)


  def removeWhiteSpace(s: String): String = s.replaceAll("\\s+", "")

  def ellipses(maxLength: Int)(s: String): String = if (s.length > maxLength) s.take(maxLength) + ".." else s

  def lastSection(marker: String)(s: String) = s.split(marker).last
  def allButlastSection(marker: String)(s: String) = s.split(marker).dropRight(1).mkString(marker)
}

