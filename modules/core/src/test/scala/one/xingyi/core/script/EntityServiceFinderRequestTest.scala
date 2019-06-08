package one.xingyi.core.script
import one.xingyi.core.UtilsSpec
import one.xingyi.core.http._
import one.xingyi.core.json.JsonWriter
import one.xingyi.core.monad.IdentityMonad

import scala.util.Failure

abstract class AbstractEntityServiceFinderRequestTest[J: JsonWriter] extends UtilsSpec {

  import EditEntityRequestFailer.EditEntityRequestFailerForThrowable

  behavior of "EntityServiceFinderRequest"

  val fromSr = implicitly[FromServiceRequest[IdentityMonad, EntityServiceFinderRequest]]

  val srWithHost = ServiceRequest(Get, Uri("/some/uri/someid"), List(Header("host", "someHost")), None)
  val srNoHost = ServiceRequest(Get, Uri("/some/uri/someid"), List(), None)

  it should "have a 'fromServiceRequest'" in {
    fromSr(srWithHost).value.get shouldBe EntityServiceFinderRequest("someHost")
  }
  it should "have a 'fromServiceRequest' which fails if there is no host" in {
    val Failure(e) = fromSr(srNoHost).value
    e.getMessage shouldBe s"No host in the request\n$srNoHost"

  }
}
