package org.validoc.utils.service

import org.validoc.utils.Futurable

case class ServiceDisplayData(typeName: String, name: String, priority: Int)

trait ServiceWithDisplayData {
  def priority = 0

  def displayData: ServiceDisplayData

  def status: Option[String]
}

abstract class WrappingService[M[_] : Futurable, Req, Res](name: String, val delegate: (_ => M[_])) extends (Req => M[Res]) with ServiceWithDisplayData {
  override def displayData: ServiceDisplayData = ServiceDisplayData(getClass.getSimpleName, name, priority)

  override def status = None
}


trait Displayable[Service] {

  def displayData(s: Service): ServiceDisplayData

  def status(s: Service): Option[String]

  def statusWithCommaPrefix(s: Service) = status(s).fold("")(status => s", $status")
}

object Displayable {

  object DisplayableForServiceWithDisplayData extends Displayable[ServiceWithDisplayData] {
    override def displayData(s: ServiceWithDisplayData): ServiceDisplayData = s.displayData

    override def status(s: ServiceWithDisplayData): Option[String] = s.status
  }

}

trait DisplayableForServiceWithDisplayData {}

case class DisplayableService[M[_] : Futurable, Req, Res, Service <: Req => M[Res] : Displayable](service: Service) extends (Req => M[Res]) {
  val displayble = implicitly[Displayable[Service]]
  val displayData = displayble.displayData(service)

  import displayData._

  def apply(req: Req) = service(req)

  override def toString(): String = s"$typeName(${displayble.displayData(service).name}${displayble.statusWithCommaPrefix(service)})"
}

trait ServiceAdder[M[_]] {
  private val lock = new Object
  private var _services = List[DisplayableService[M, _, _, _]]()

  protected def typeName: String

  def addService[Req, Res, Service <: Req => M[Res] : Displayable](service: Service)(implicit futurable: Futurable[M]) =
    lock.synchronized {
      val result = DisplayableService[M, Req, Res, Service](service)
      _services = result :: _services
      result
    }
}
