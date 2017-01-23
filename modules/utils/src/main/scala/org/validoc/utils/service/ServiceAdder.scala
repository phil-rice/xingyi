package org.validoc.utils.service

import org.validoc.utils.concurrency.Futurable

trait ServiceAdder[M[_]] {
  private val lock = new Object
  private var _services = List[DisplayableService[M, _, _, _]]()

  protected def typeName: String

  def addService[Req, Res, Service <: Req => M[Res] : Displayable](service: Service)(implicit futurable: Futurable[M]) =
    lock.synchronized {
      val result = DisplayableService[M, Req, Res, Service](service)
      require(result.displayData.typeName == typeName, s"Service should have typename $typeName has ${result.displayData.typeName}. Service has class ${service.getClass} and toString $service")
      _services = result :: _services
      result
    }
}
