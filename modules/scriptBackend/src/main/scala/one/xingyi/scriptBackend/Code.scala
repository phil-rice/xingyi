package one.xingyi.scriptBackend
import one.xingyi.core.http._
import one.xingyi.core.monad.Monad
import one.xingyi.core.language.Language._
import one.xingyi.core.script.{DomainDetails, Javascript}
import one.xingyi.core.strings.Strings

import scala.language.higherKinds

case class CodeRequest()
object CodeRequest {
  implicit def fromServiceRequest[M[_] : Monad]: FromServiceRequest[M, CodeRequest] = { sr => CodeRequest().liftM[M] }
}
case class Code[T](domainDetails: DomainDetails[T])
object Code {
  implicit def toServiceResponse[T]: ToServiceResponse[Code[T]] = {
    code =>
      val s = code.domainDetails.code.toList.sortBy(_._1.toString).map { case (hash, code) => code.hash + "\n" + code.code + "\n\n\n" }.mkString("\n")
      ServiceResponse(Status(200), Body(s), ContentType("text/plain"))
  }

}
case class CodeDetailsRequest(hash: String)
object CodeDetailsRequest {
  implicit def fromServiceRequest[M[_] : Monad]: FromServiceRequest[M, CodeDetailsRequest] = { sr => CodeDetailsRequest(Strings.lastSection("/")(sr.uri.path.path)).liftM[M] }
}

case class CodeDetailsResponse(code: String)
object CodeDetailsResponse {
  implicit def toServiceResponse: ToServiceResponse[CodeDetailsResponse] = cdr => ServiceResponse(Status(200), Body(cdr.code), ContentType("text/plain"))
}

