/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.script
import one.xingyi.core.UtilsSpec

class IXingYiHeaderToLensNamesTest extends UtilsSpec {

  val toLensnames = implicitly[IXingYiHeaderToLensNames]

  behavior of "IXingYiHeaderToLensNames"

  it should "return an empty set if there is no accept header" in {
    toLensnames(None) shouldBe Set()
  }

  it should "return an empty set if there is an accept header but no application/xingyi...." in {
    toLensnames(Some("not interesting stuff")) shouldBe Set()
  }

  it should "blow up if the string with appliction/axingyi isn't at the start" in {
    intercept[RuntimeException](toLensnames(Some("someprefix application/xingyi.lens_person_line1_string,lens_person_line2_string")))

  }
  it should "rip apart a valid application/xingyi header" in {
    toLensnames(Some("application/xingyi.lens_person_line1_string,lens_person_line2_string")) shouldBe Set("lens_person_line1_string", "lens_person_line2_string")
  }
}
