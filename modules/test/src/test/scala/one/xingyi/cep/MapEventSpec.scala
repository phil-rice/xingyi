/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cep
import one.xingyi.cep.model.Event
import one.xingyi.core.UtilsSpec

import scala.language.reflectiveCalls
abstract class AbstractMapEventSpec[ED: CEP] extends UtilsSpec with CepFixture[ED] {

}

import one.xingyi.cep.CEPSpec._
class MapEventSpec extends AbstractMapEventSpec[StringMap] {
  behavior of "MapEvent"
  import pp2.map123
  it should "have fields" in {
    map123.fields.map(_.name) shouldBe List("ipaddress", "type", "businessEventSubtype")
  }
  it should "be able to calculate values for the fields" in {

    val map = Map[Event, StringMap](
      pp2.ie1 -> Map("customerId" -> "someValue", "type" -> "A", "ipaddress" -> "ip1"),
      pp2.ie2 -> Map("customerId" -> "someValue", "type" -> "B", "ipaddress" -> "ip2"),
      pp2.ie3 -> Map("customerId" -> "someValue", "type" -> "C", "ipaddress" -> "ip3")
    )
    val last = new LastEventAndDataForTest(map123, map)
    implicit val notUsedButNeedForCompileUntilGetMacrosSorted: LastEventAndData = mock[LastEventAndData]
    map123.findDataForThisEvent(last)
    map123.ipaddress.value shouldBe "ip1/ip2/ip3"
    map123.`type`.value shouldBe "A-B-C"
    map123.businessEventSubtype.value shouldBe "performance-test-data"
  }
}
