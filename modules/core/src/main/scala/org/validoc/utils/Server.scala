package org.validoc.utils

import org.validoc.utils.serviceTree.{ServiceDescription, ServiceTree}
import scala.language.higherKinds
trait Closable[T] {
  def close(t: T)
}

trait MakeServer[M[_], HttpReq, HttpRes, S <: HttpReq => M[HttpRes]] {
  def start(serverPort: Int)(serviceTree: ServiceTree[M, _, _, ServiceDescription])(implicit closable: Closable[S]): S
}
