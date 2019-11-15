package one.xingyi.kwikServer

import scala.io.Source

object Variable {
  def parse(s: String): Variable = {
    try {
      val Array(name, value) = s.split(" ")
      Variable(name, value)
    } catch {
      case e: MatchError => throw new RuntimeException(s"expected a string with a name and a value separated by a space. Actually had [$s]")
    }
  }

  def parseList(expectedTitle: String)(s: Iterator[String]): List[Variable] =
    TitleLengthValue.parseAndValidate(expectedTitle)(s).value.map(Variable.parse)
}

case class Variable(name: String, value: String)


case class TitleLengthValue(title: String, value: List[String]) {
  def length = value.size

  override def toString: String = title + " " + length + "\n" + value.mkString("\n")
}

object TitleLengthValue {
  def fromStrings(title: String, s: String) = TitleLengthValue(title, Source.fromString(s).getLines().toList)

  private def parsePrim(expectedTitle: Option[String])(lines: Iterator[String]): TitleLengthValue = {
    val titleAndLength = lines.next()
    try {
      val Array(title, length) = titleAndLength.split(" ")
      expectedTitle.foreach(expected => if (title != expected) throw new RuntimeException(s"expected $expectedTitle actual $title"))
      val pom = lines.take(length.toInt).toList
      TitleLengthValue(title, pom)
    } catch {
      case e: MatchError => throw new RuntimeException(s"expected a string with a name and a value separated by a space. Actually had [$titleAndLength]")
    }
  }

  def parse: Iterator[String] => TitleLengthValue = parsePrim(None)

  def parseAndValidate(expectedTitle: String): Iterator[String] => TitleLengthValue = parsePrim(Some(expectedTitle))

  def parseList(s: Iterator[String]): Seq[TitleLengthValue] = {
    var result = List[TitleLengthValue]()
    while (s.hasNext)
      result = result :+ TitleLengthValue.parse(s)
    result
  }
}
