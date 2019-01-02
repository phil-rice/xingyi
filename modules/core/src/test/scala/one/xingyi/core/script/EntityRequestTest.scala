package one.xingyi.core.script
import one.xingyi.core.UtilsSpec
import one.xingyi.core.http._
import one.xingyi.core.json.{JsonParser, JsonWriter, ObjectProjection}
import one.xingyi.core.monad.IdentityMonad

import scala.util.Failure

abstract class EntityRequestTest[J: JsonParser: JsonWriter, SharedE, DomainE](implicit projection: ObjectProjection[SharedE, DomainE]) extends UtilsSpec {

  import EditEntityRequestFailer.EditEntityRequestFailerForThrowable

  behavior of "Entity Request"

  val srWithHost = ServiceRequest(Get, Uri("/some/uri/someid"), List(Header("host", "someHost")), None)
  val srNoHost = ServiceRequest(Get, Uri("/some/uri/someid"), List(), None)
  val fromSr = implicitly[FromServiceRequest[IdentityMonad, EntityRequest]]

  it should "be gettable from a Service request" in {
    fromSr(srWithHost).value.get shouldBe EntityRequest("someid", "someHost")
  }

  it should "have a fail if no host" in {
    val Failure(e) = fromSr(srNoHost).value
    e.getMessage shouldBe s"No host in the request\n$srNoHost"
  }

}
