package org.validoc.utils.http

import org.validoc.utils.concurrency.Async
import org.validoc.utils.endpoint.EndPointService
import org.validoc.utils.serviceTree.{RootServiceTree, ServiceDescription, ServiceLanguageExtension}

import scala.language.higherKinds
import scala.reflect.ClassTag


trait MakeHttpService[M[_], HttpReq, HttRes] extends (ServiceName => (HttpReq => M[HttRes]))


trait HttpServiceLanguageExtension[M[_], HttpReq, HttpRes] extends ServiceLanguageExtension[M] {
  def http(serviceName: ServiceName)(implicit makeHttpService: MakeHttpService[M, HttpReq, HttpRes], httpReqClassTag: ClassTag[HttpReq], httpResClassTag: ClassTag[HttpRes]): RootServiceTree[M, HttpReq, HttpRes, ServiceDescription] =
    root[HttpReq, HttpRes](s"Http($serviceName)", () => makeHttpService(serviceName))
}


object MakeHttpService {
  def apply[M[_], HttpReq, HttpRes](map: Map[ServiceName, (HttpReq => M[HttpRes])]) = new MakeHttpService[M, HttpReq, HttpRes] {
    override def apply(v1: ServiceName) = map(v1)
  }

}