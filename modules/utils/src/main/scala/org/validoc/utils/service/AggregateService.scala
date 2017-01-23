package org.validoc.utils.service

import org.validoc.utils.Service


abstract class AggregateService[M[_], Req, Res](val name: String, val children: Seq[Service[M, Req, Res]])
  extends Service[M, Req, Res]
    with ServiceWithDisplayData
    with ServiceWithChildren[M] {

  override def displayData: ServiceDisplayData = ServiceDisplayData(getClass.getSimpleName, name, priority)

  override def status = None

}
