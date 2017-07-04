package org.validoc

import org.validoc.utils.service.ServerContext

import scala.language.higherKinds

package object utils {

  type Service[M[_], Req, Res] = (Req => M[Res])
  type Parser[T] = String => T



}