package one.xingyi.core.orm

import java.io.OutputStream
import java.text.MessageFormat
import java.util.Date

import com.sun.tools.javac.util.Context
import one.xingyi.core.json.{GetFromJson, JsonObject, JsonParser, JsonString, JsonValue, ToJsonLib, WriteToJson}
import one.xingyi.core.parserAndWriter.{Parser, Writer}
import one.xingyi.core.strings.Strings

import scala.language.higherKinds


case class LinkUrl(url: String) extends AnyVal

trait LinkPrefixFrom[Context] {def apply(c: Context): List[String]}
trait MakeContextFromLink[Context] {def apply(url: String): Context}

object LinkUrl {

  def apply[Context, Schema[_]](c: Context, s: Schema[LinkUrl], list: List[String])(implicit linkPrefix: LinkPrefixFrom[Context], getLinkPattern: GetPattern[Schema]): LinkUrl = {
    val pattern = getLinkPattern.getOrException(s, classOf[LinkUrl].getSimpleName)
    LinkUrl(MessageFormat.format(pattern, (linkPrefix(c) ::: list): _*))
  }

  implicit object ParserForLinkUrl extends Parser[LinkUrl] {def apply(s: String): LinkUrl = LinkUrl(s)}
  implicit object WriterForLinkUrl extends Writer[LinkUrl] {def apply(s: LinkUrl) = s.url}
  implicit def WriteToJsonForLinkUrl: WriteToJson[LinkUrl] = (t: LinkUrl) => JsonString(t.url)

  implicit def getFromJsonForLinkUrl: GetFromJson[LinkUrl] = new GetFromJson[LinkUrl] {
    override def apply[Schema[_] : GetPattern, J](s: Schema[LinkUrl], j: J)(implicit jsonParser: JsonParser[J]): LinkUrl = GetFromJson.wrap(j)(j => LinkUrl(jsonParser.extractString(j)))
  }

  implicit def toFieldTypeForLinkUrl: ToFieldType[LinkUrl] = new ToFieldType[LinkUrl] {
    override def apply(name: String): FieldType[LinkUrl] = FieldType(name, "varchar(255", numericSort = false)
  }

  implicit def ValueFromMultipleTableFieldsForMultipleFieldData[Schema[_]](implicit getPatternFrom: GetPattern[Schema]): ValueFromMultipleAliasFields[LinkUrl] = {
    new ValueFromMultipleAliasFields[LinkUrl] {
      override def apply[Context: LinkPrefixFrom, HasPattern[_] : GetPattern](context: Context, schema: HasPattern[LinkUrl], fieldTypeToIndex: FieldTypeToIndex, fieldTypes: List[FieldType[_]]): List[Any] => LinkUrl =
        oneRow => LinkUrl(context, schema, fieldTypes.map(ft => oneRow(fieldTypeToIndex.fieldTypeToIndex(ft)).toString))
    }
  }

  implicit def jsonToStreamForLinkUrl[Schema[_]]: JsonToStream[Schema, LinkUrl] = new JsonToStream[Schema, LinkUrl] {
    override def put(f: Schema[LinkUrl], t: Any, stream: OutputStream): Unit =
      t match {
        case o: LinkUrl =>
          JsonToStream.putUnescaped(stream, """{"href":"""")
          JsonToStream.putUnescaped(stream, o.url)
          JsonToStream.putUnescaped(stream, """"}""")
      }
  }

}

