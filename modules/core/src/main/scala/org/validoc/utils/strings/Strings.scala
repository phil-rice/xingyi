package org.validoc.utils.strings

object Strings {
  def classNameOfObject(obj: Object) = obj.getClass.getSimpleName.dropRight(1)


  def removeWhiteSpace(s: String): String = s.replaceAll("\\s+", "")

  def ellipses(maxLength: Int)(s: String): String = if (s.length > maxLength) s.take(maxLength) + ".." else s

  def lastSection(marker: String)(s: String) = s.split(marker).last
  def allButlastSection(marker: String)(s: String) = s.split(marker).dropRight(1).mkString(marker)
}


//case class Indent(s: String = "  ", size: Int = 0) {
//  def indent = Indent(s, size + 1)
//
//  def unIndent = Indent(s, Math.max(size - 1, 0))
//
//  override def toString = List.fill(size)(s).mkString("")
//
//  def padTo(target: Int, ch: Char = ' ') = List.fill(Math.max(0, target - size * s.length))(ch).mkString("")
//
//  def nameValue(maxNameWidth: Int, ch: Char = ' ')(name: String, value: Option[String]) = {
//    val nameString = Strings.ellipses(maxNameWidth - 3)(indent + name)
//    value.fold(nameString) { value =>
//      val padding = List.fill(Math.max(0, maxNameWidth - nameString.length))(ch).mkString("")
//      nameString + padding + value
//    }
//  }
//}