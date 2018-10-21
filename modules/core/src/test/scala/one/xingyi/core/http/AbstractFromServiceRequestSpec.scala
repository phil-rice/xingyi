package one.xingyi.core.http
import one.xingyi.core.UtilsSpec
import one.xingyi.core.language.Language._
import one.xingyi.core.monad.{Async, Monad}

import scala.language.higherKinds
import scala.reflect.ClassTag

abstract class AbstractFromServiceRequestSpec[M[_] : Async, Res: ClassTag](implicit monad: Monad[M], fromServiceRequest: FromServiceRequest[M, Res]) extends UtilsSpec {

  def checkMethod(res: Res, sr: ServiceRequest)
  def checkHeaders(res: Res, sr: ServiceRequest)
  def checkBody(res: Res, sr: ServiceRequest)
  def checkUri(res: Res, sr: ServiceRequest)

  val headers = List(Header("one", "valueOne"))

  val sr = ServiceRequest(Get, Uri("/someUri?a=1&b=2"), headers, Some(Body("body")))
  val srNoBody = ServiceRequest(Get, Uri("/someUri?a=1&b=2"), headers, None)

  behavior of "FromServiceRequest for " + implicitly[ClassTag[Res]].runtimeClass.getName

  it should "turn a service request into a Res - with body" in {
    val res = fromServiceRequest(sr).await()
    checkMethod(res, sr)
    checkHeaders(res, sr)
    checkBody(res, sr)
    checkUri(res, sr)
  }

  it should "turn a service request into a Res - with out body" in {
    val res = fromServiceRequest(srNoBody).await()
    checkMethod(res, srNoBody)
    checkHeaders(res, srNoBody)
    checkBody(res, srNoBody)
    checkUri(res, srNoBody)
  }
}
