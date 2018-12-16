package one.xingyi.scriptBackend
import one.xingyi.core.http._
import one.xingyi.core.monad.Monad
import one.xingyi.core.language.Language._
case class CodeRequest()
object CodeRequest {
  implicit def fromServiceRequest[M[_] : Monad]: FromServiceRequest[M, CodeRequest] = { sr => CodeRequest().liftM[M] }
}

case class Code(javascript: String, scala: String)
object Code {
  implicit def toServiceResponse: ToServiceResponse[Code] = code => ServiceResponse(Status(200), Body(code.javascript + "\n\n" + code.scala), ContentType("text/plain"))

}
