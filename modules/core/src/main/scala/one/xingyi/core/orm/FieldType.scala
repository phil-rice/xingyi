/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.orm

import one.xingyi.core.json.{GetFromJson, JsonInt, JsonString, JsonValue, WriteToJson}
import one.xingyi.core.strings.{ParseException, Strings}

import scala.reflect.ClassTag

case class FieldType[T](name: String, typeName: String)(implicit val writeToJson: WriteToJson[T], val getFromJson: GetFromJson[T], val classTag: ClassTag[T])
object FieldType {
  private val splitter = Strings.splitInTwo(":")
  def parse(s: String): FieldType[_] = if (s.contains(":")) splitter(s) match {
    case (l, "int") => int(l)
    case (l, "string") => string(l)
    case (l, r) => throw new ParseException(s"Cannot work out what type of field ${l} is. Its type is [$r] and not int or string")
  } else string(s)

  def nameAndTypeName[T](ft: FieldType[T]): String = ft.name + " " + ft.typeName
  def string(name: String) = FieldType[String](name, "varchar(255)")
  def int(name: String) = FieldType[Int](name, "integer")
}

