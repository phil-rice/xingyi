/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cep.model

import one.xingyi.cep.exceptions.CannotgetData
import one.xingyi.cep.{LastEventAndData, StringFieldGetter, ValueFn}
import one.xingyi.core.builder.{Aggregator, HasAggregator, HasId}

import scala.language.experimental.macros

object StringField {
  implicit object hasIdForStringField extends HasId[StringField, String] {override def apply(v1: StringField): String = v1.name}
}

abstract class StringField(implicit val aggregator: Aggregator[StringField]) extends HasAggregator[StringField] {
  aggregator(this)
  def name: String
  def event: Event
  def :=(value: String): StringField = macro Macros.assignmentImpl
  def value(implicit lastEventAndData: LastEventAndData) =
    try {lastEventAndData.data(event)(name)} catch {case e: Exception => throw new CannotgetData(this, lastEventAndData, name, event, e)}
  def get[ED](ed: ED)(implicit stringFieldGetter: StringFieldGetter[ED]): Option[String] = stringFieldGetter(this)(ed)
  override def toString: String = s"${getClass.getSimpleName}( $event,  $name)"
}

case class KeyByStringField(name: String) extends StringField()(Aggregator.nullAggregator[StringField]) {def event = NullEvent}

case class SimpleStringField(event: Event, name: String)(implicit aggregator: Aggregator[StringField]) extends StringField
case class StringFieldWithValue(event: Event, name: String, valueFn: ValueFn)(implicit aggregator: Aggregator[StringField]) extends StringField {
  override def value(implicit lastEventAndData: LastEventAndData) = valueFn(lastEventAndData.data)
}

