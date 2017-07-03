package org.validoc.utils.service
import scala.language.higherKinds

trait MakeServiceMakerForClass[ServiceOld, Service] {
  def apply(delegate: ServiceOld): Service
}

trait MakeServiceMakerForClassWithParam[Param, ServiceOld, Service] {
  def apply(param: Param, delegate: ServiceOld):Service
}



