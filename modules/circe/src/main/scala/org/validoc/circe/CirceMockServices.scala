package org.validoc.circe

import scala.reflect.ClassTag
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import org.validoc.utils.either.Eithers._
import io.circe.syntax._
import org.validoc.utils.concurrency.Async
import Async._
import org.validoc.utils.http.{ServiceRequest, ServiceResponse}


//
//class CirceMockServices[M[_]: Async, Req: ClassTag, Res: ClassTag](pf: PartialFunction[Req, Res])extends PartialFunction[ServiceRequest, M[ServiceResponse]])  {
//
//  override def apply(req: Req) = {
//
//  }
//
//}
