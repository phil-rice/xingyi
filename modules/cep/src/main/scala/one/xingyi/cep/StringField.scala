package one.xingyi.cep
import one.xingyi.core.builder.{Aggregator, HasAggregator, HasId}
import one.xingyi.core.reflection.Macros
import scala.language.experimental.macros

object StringField {
  implicit object hasIdForStringField extends HasId[StringField, String] {override def apply(v1: StringField): String = v1.name}
}

abstract class StringField(implicit val aggregator: Aggregator[StringField]) extends HasAggregator[StringField] {
  aggregator(this)
  def name: String
  def event: Event
  def :=(value: String): StringField = macro Macros.assignmentImpl
  def value(implicit lastEventAndData: LastEventAndData) = try {lastEventAndData.data(event)(name)} catch {case e: Exception => throw new CannotgetData(this, lastEventAndData, name, event, e)}
  def get[ED](ed: ED)(implicit stringFieldGetter: StringFieldGetter[ED]) = stringFieldGetter.getString(this)(ed)
  override def toString: String = s"${getClass.getSimpleName}( $event,  $name)"
}

case class KeyByStringField(name: String) extends StringField()(Aggregator.nullAggregator[StringField]) {def event = NullEvent}

case class SimpleStringField(event: Event, name: String)(implicit aggregator: Aggregator[StringField]) extends StringField
case class StringFieldWithValue(event: Event, name: String, valueFn: ValueFn)(implicit aggregator: Aggregator[StringField]) extends StringField {
  override def value(implicit lastEventAndData: LastEventAndData) = valueFn(lastEventAndData.data)
}

class CannotgetData(stringField: StringField, lastEventAndData: LastEventAndData, name: String, event: Event, cause: Throwable) extends RuntimeException(
  s"""
     |stringField: $stringField
     |name:  $name
     |event: $event
     |Data:
     |$lastEventAndData
  """.stripMargin, cause)

