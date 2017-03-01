package org.validoc


package object utils {

  type Service[M[_], Req, Res] = (Req => M[Res])
  type Parser[T] = String => T
}