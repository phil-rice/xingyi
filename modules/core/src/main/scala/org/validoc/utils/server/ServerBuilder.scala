package org.validoc.utils.server

import org.validoc.utils.concurrency.Async
import org.validoc.utils.serviceTree.{ServiceDescription, ServiceTree}
import scala.language.higherKinds

trait ServerBuilder[M[_], T] {
  def apply(port: Int, serviceTrees: List[ServiceTree[M, _, _, ServiceDescription]]): T
}
