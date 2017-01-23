package org.validoc.utils.service

import org.validoc.utils.Service
import org.validoc.utils.concurrency.Futurable


abstract class WrappingService[M[_] : Futurable, Req, Res](name: String, val delegate:Service[M,_,_])
  extends Service[M,Req,Res]
    with ServiceWithDisplayData
    with ServiceWithChildren[M] {
  override def displayData: ServiceDisplayData = ServiceDisplayData(getClass.getSimpleName, name, priority)

  override def status = None

  override def children = Seq(delegate)
}
