/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.orm

import java.util.Date

import one.xingyi.core.json.{GetFromJson, WriteToJson}
import one.xingyi.core.strings.{ParseException, Strings}

import scala.reflect.ClassTag

trait ToFieldType[T] {
  def apply(name: String): FieldType[T]
}
object ToFieldType {
  implicit def toFieldTypeForString: ToFieldType[String] = FieldType[String](_, "varchar(255)", false)
  implicit def toFieldTypeForInt: ToFieldType[Int] = FieldType[Int](_, "integer", true)
  //  implicit def toFieldTypeForDate: ToFieldType[Date] = FieldType[Date](_, "date", true)
}
case class FieldTypeAndIndex[T](fieldType: FieldType[T], index: Int)

case class FieldType[T](name: String, typeName: String, numericSort: Boolean)(implicit val writeToJson: WriteToJson[T], val getFromJson: GetFromJson[T], val classTag: ClassTag[T]) {
  def withIndex(list: List[String]): FieldTypeAndIndex[T] = {
    val i = list.indexOf(name)
    if (i < 0) throw new RuntimeException(s"Cannot find index of $name in $list")
    FieldTypeAndIndex(this, i)
  }
  def withIndex(index: Int): FieldTypeAndIndex[T] = FieldTypeAndIndex(this, index)
  def prettyPrint = s"$name/$typeName"
}
object FieldType {
  private val splitter = Strings.splitInTwo(":")
  /** The s can be 'key' (which makes string) or 'key:int', 'key:string'.
   * This DSL is a place that needs improvement */
  def apply[T](s: String): FieldType[T] = if (s.contains(":")) splitter(s) match {
    case (l, "int") => int(l).asInstanceOf[FieldType[T]]
    case (l, "string") => string(l).asInstanceOf[FieldType[T]]
    //    case (l, "date") => string(l).asInstanceOf[FieldType[T]]
    case (l, r) => throw new ParseException(s"Cannot work out what type of field ${l} is. Its type is [$r] and not int or string")
  } else string(s).asInstanceOf[FieldType[T]]

  def nameAndTypeName[T](ft: FieldType[T]): String = ft.name + " " + ft.typeName
  def string(name: String)(implicit toFieldType: ToFieldType[String]) = toFieldType(name)
  def date(name: String)(implicit toFieldType: ToFieldType[Date]) = toFieldType(name)
  def int(name: String)(implicit toFieldType: ToFieldType[Int]) = toFieldType(name)
}
