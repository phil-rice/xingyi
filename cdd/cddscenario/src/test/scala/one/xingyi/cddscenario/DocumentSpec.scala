/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddscenario
import one.xingyi.core.UtilsSpec

class DocumentSpec  extends UtilsSpec{

  behavior of "Document"

  it should "have helper methods that allow easy creation of internet and paper documents" in {
    Document.internet("someRef") shouldBe InternetDocument("someRef", "someRef")
    Document.internet("someName", "someRef") shouldBe InternetDocument("someName", "someRef")
    Document.paper("someName") shouldBe PaperDocument("someName", "")
    Document.paper("someName", "someRef") shouldBe PaperDocument("someName", "someRef")
  }

  behavior of "Reference"
  it should "have a helper method that allows a reference to be easily made" in {
    val doc = InternetDocument("someName", "someRef")
    Reference(doc) shouldBe Reference(doc, None)
    Reference(doc, "internalRef") shouldBe Reference(doc, Some("internalRef"))
  }
}
