package one.xingyi.core.http
import one.xingyi.core.UtilsSpec

import scala.reflect.ClassTag

abstract class AbstractToServiceResponseSpec[T: ClassTag](implicit toServiceResponse: ToServiceResponse[T]) extends UtilsSpec {

  def makeT: T

  behavior of s"toServiceResponse for ${implicitly[ClassTag[T]].runtimeClass.getName}"

  it should "turn a T to a ServiceResponse" in {
    toServiceResponse(makeT) shouldBe ServiceResponse(Status(200), Body("someBody"), List(Header("header1", "value1")))
  }

}
