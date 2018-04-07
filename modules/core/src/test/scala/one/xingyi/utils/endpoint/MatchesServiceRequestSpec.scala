package one.xingyi.utils.endpoint

import one.xingyi.utils.UtilsSpec
import one.xingyi.utils.http._

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
