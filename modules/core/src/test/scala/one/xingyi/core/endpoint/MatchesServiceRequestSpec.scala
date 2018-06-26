/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.endpoint

import one.xingyi.core.UtilsSpec
import one.xingyi.core.http._

trait ServiceRequestForEndpointFixture{
  val srGetPath = ServiceRequest(Get, Uri("/some/path?a=1"), body = Some(Body("someGetPathBody")))
  val srPutPath = ServiceRequest(Put, Uri("/some/path?a=1"), body = Some(Body("somePutPathBody")))
  val srGetPathWithId = ServiceRequest(Get, Uri("/some/path/someId?a=1"), body = Some(Body("someGetPathBody")))
  val srPutPathWithId = ServiceRequest(Put, Uri("/some/path/someId?a=1"), body = Some(Body("somePutPathBody")))


}

class MatchesServiceRequestSpec extends UtilsSpec with ServiceRequestForEndpointFixture {

  behavior of "fixedPath"

  it should "match verb and path" in {
    MatchesServiceRequest.fixedPath(Get)("/some/path")(srGetPath) shouldBe true
    MatchesServiceRequest.fixedPath(Put)("/some/path")(srPutPath) shouldBe true

    MatchesServiceRequest.fixedPath(Get)("/some/path1")(srGetPath) shouldBe false
    MatchesServiceRequest.fixedPath(Post)("/some/path")(srGetPath) shouldBe false
  }

  behavior of "id at end"

  it should "match prefix " in {
    MatchesServiceRequest.idAtEnd(Get)("/some/path")(srGetPathWithId) shouldBe true
    MatchesServiceRequest.idAtEnd(Put)("/some/path")(srPutPathWithId) shouldBe true

    MatchesServiceRequest.idAtEnd(Put)("/some/path1")(srPutPathWithId) shouldBe false
    MatchesServiceRequest.idAtEnd(Put)("/some/path")(srPutPath) shouldBe false
  }

}
