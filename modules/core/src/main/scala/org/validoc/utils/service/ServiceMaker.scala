package org.validoc.utils.service

import scala.language.higherKinds

trait MakeServiceMakerForClass[ServiceOld, Service] extends (ServiceOld => Service)

trait MakeServiceMakerForClassWithParam[Param, ServiceOld, Service] extends ((Param, ServiceOld) => Service)

trait MakeServiceMakerForTwoServices[ServiceOld1, ServiceOld2, Service] extends ((ServiceOld1, ServiceOld2) => Service)





