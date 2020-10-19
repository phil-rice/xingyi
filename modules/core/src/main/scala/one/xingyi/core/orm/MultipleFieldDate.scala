package one.xingyi.core.orm

import java.io.OutputStream
import java.text.SimpleDateFormat
import java.util.Date

import com.sun.tools.javac.util.Context
import one.xingyi.core.json.{GetFromJson, JsonParser, JsonString, WriteToJson}
import one.xingyi.core.parserAndWriter.{Parser, Writer}

import scala.language.higherKinds

trait GetDateFormatter {def apply(format: String): ThreadLocal[SimpleDateFormat]}
object GetDateFormatter {
  implicit object GetDateFormatter extends GetDateFormatter {
    var map = Map[String, ThreadLocal[SimpleDateFormat]]()
    val lock = new Object
    override def apply(format: String): ThreadLocal[SimpleDateFormat] = if (map.contains(format)) map(format) else {
      val result = new ThreadLocal[SimpleDateFormat] {override def initialValue(): SimpleDateFormat = new SimpleDateFormat(format)}
      lock.synchronized { map = map + (format -> result) }
      result
    }
  }
}

object MultipleFieldDate {
  val dateFormatter: ThreadLocal[SimpleDateFormat] = new ThreadLocal[SimpleDateFormat] {override def initialValue(): SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd")}

  implicit object ParserForFourDate extends Parser[MultipleFieldDate] {def apply(s: String): MultipleFieldDate = MultipleFieldDate(dateFormatter.get().parse(s), dateFormatter)}

  implicit object WriterForFourDate extends Writer[MultipleFieldDate] {def apply(s: MultipleFieldDate): String = dateFormatter.get().format(s.date)}
  implicit def WriteToJsonForFourDate: WriteToJson[MultipleFieldDate] = (t: MultipleFieldDate) => JsonString(t.dateFormatter.get.format(t.date))
  implicit def getFromJsonForFourDate(implicit getDateFormatter: GetDateFormatter): GetFromJson[MultipleFieldDate] = new GetFromJson[MultipleFieldDate] {
    override def apply[Schema[_] : GetPattern, J](s: Schema[MultipleFieldDate], j: J)(implicit jsonParser: JsonParser[J]): MultipleFieldDate =
      GetFromJson.wrap(j) { j =>
        val dateFormatter = getDateFormatter(implicitly[GetPattern[Schema]].getOrException(s, classOf[MultipleFieldDate].getSimpleName))
        MultipleFieldDate(dateFormatter.get.parse(jsonParser.extractString(j)), dateFormatter)
      }
  }
  implicit def toFieldTypeForMultipleDate(implicit getDateFormatter: GetDateFormatter): ToFieldType[MultipleFieldDate] = FieldType(_, "fourdate", numericSort = false)
  implicit def toJsonStreamForFourDate[F[_]]: JsonToStream[F, MultipleFieldDate] = new JsonToStream[F, MultipleFieldDate] {
    override def put(f: F[MultipleFieldDate], t: Any, stream: OutputStream): Unit =
      JsonToStream.putEscapedWithQuotes(dateFormatter.get.format(t.asInstanceOf[MultipleFieldDate].date), stream)
  }

  implicit def ValueFromMultipleTableFieldsForMultipleFieldData(implicit getDateFormatter: GetDateFormatter): ValueFromMultipleAliasFields[MultipleFieldDate] =
    new ValueFromMultipleAliasFields[MultipleFieldDate] {
      override def apply[Context: ZerothValueFromContext, Schema[_] : GetPattern](context: Context, schema: Schema[MultipleFieldDate], fieldTypeToIndex: FieldTypeToIndex, fieldTypes: List[FieldType[_]]): List[Any] => MultipleFieldDate = { oneRow =>
        require(fieldTypes.size == 3 || fieldTypes.size == 4, s"Cannot apply ValueFromMultipleTableFieldsForMultipleFieldData as the listsize is not 3 or 4.\nList is ${fieldTypes.map(_.name)}\nfull ${fieldTypes}")
        val dd = oneRow(fieldTypeToIndex.fieldTypeToIndex(fieldTypes(0))).toString
        val mm = oneRow(fieldTypeToIndex.fieldTypeToIndex(fieldTypes(1))).toString
        val yy = oneRow(fieldTypeToIndex.fieldTypeToIndex(fieldTypes(2))).toString
        val cc = if (fieldTypes.size == 3) "" else oneRow(fieldTypeToIndex.fieldTypeToIndex(fieldTypes(3)))
        val formatter = getDateFormatter(implicitly[GetPattern[Schema]].getOrException(schema, classOf[MultipleFieldDate].getSimpleName))
        MultipleFieldDate(formatter.get().parse(cc + yy + "-" + mm + "-" + dd), formatter)
      }
    }

}


case class MultipleFieldDate(date: Date, dateFormatter: ThreadLocal[SimpleDateFormat]) {
  override def toString: String = s"MultipleFieldDate(${dateFormatter.get().format(date)})"
  override def hashCode(): Int = date.hashCode()
  override def equals(obj: Any): Boolean = obj match {case o: MultipleFieldDate => o.date == date; case _ => false}
}
