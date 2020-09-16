/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.orm

import one.xingyi.core.UtilsSpec

import scala.language.implicitConversions

trait EntityFixture {
  import FieldType._

  val personIdField = int("id")
  val nameField = string("name")

  val addressIdField = int("id")
  val addressPersonIdField = int("personId")
  val line1Field = string("line1")
  val line2Field = string("line2")


  val phoneIdField = int("id")
  val phonePersonIdField = int("personId")
  val manufacturerField = string("manufacturer")

  val phoneDetailsIdField = int("id")
  val purposeField = string("purpose")
  val telField = string("tel")

  implicit def fieldToKeys[T](f: FieldType[T]) = Keys(List(f))
  val addressEntity = OneToManyEntity("address", "a", addressIdField, addressPersonIdField, List(line1Field, line2Field), List())
  val phoneDetailsEntity = OneToManyEntity("phonedetails", "pd", phoneDetailsIdField, phonePersonIdField, List(purposeField, telField), List())
  val phoneEntity = OneToManyEntity("phone", "ph", phoneIdField, phonePersonIdField, List(manufacturerField), List(phoneDetailsEntity))
  val mainEntity = MainEntity("person", "p", personIdField, List(nameField), List(addressEntity, phoneEntity))
}

class EntitySpec extends UtilsSpec with EntityFixture {

  behavior of "MainEntity"

  it should "have a fieldsForCreate which is the primary key and the data fields" in {
    mainEntity.fieldsForCreate shouldBe List(personIdField, nameField)
  }

  behavior of "OneToManyEntity"
  it should "have a fieldsForCreate which is the primary key the parent field and the data fields" in {
    addressEntity.fieldsForCreate shouldBe List(addressIdField, addressPersonIdField, line1Field, line2Field)
    phoneEntity.fieldsForCreate shouldBe List(phoneIdField, phonePersonIdField, manufacturerField)
    phoneDetailsEntity.fieldsForCreate shouldBe List(phoneDetailsIdField, phonePersonIdField, purposeField, telField)
  }
}
