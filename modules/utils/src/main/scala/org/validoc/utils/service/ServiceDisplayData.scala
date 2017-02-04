package org.validoc.utils.service

import org.validoc.utils.concurrency.Async

case class ServiceDisplayData(typeName: String, name: String, priority: Int)

trait ServiceWithDisplayData {
  def priority = 0

  def displayData: ServiceDisplayData

  def status: Option[String]

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

case class DisplayableService[M[_] : Async, Req, Res, Service <: Req => M[Res] : Displayable](service: Service) extends (Req => M[Res]) {
  val displayble = implicitly[Displayable[Service]]
  val displayData = displayble.displayData(service)

  import displayData._

  def apply(req: Req) = service(req)

  override def toString(): String = s"$typeName(${displayble.displayData(service).name}${displayble.statusWithCommaPrefix(service)})"
}


