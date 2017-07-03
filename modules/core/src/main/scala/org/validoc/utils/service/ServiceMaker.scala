package org.validoc.utils.service

import scala.language.higherKinds

trait MakeServiceMakerForClass[ServiceOld, Service] {
  def apply(delegate: ServiceOld): Service
}

trait MakeServiceMakerForClassWithParam[Param, ServiceOld, Service] {
  def apply(param: Param, delegate: ServiceOld): Service
}

trait MakeServiceMakerForTwoServices[ServiceOld1, ServiceOld2, Service] {
  def apply(old1: ServiceOld1, old2: ServiceOld2): Service
}





