package org.validoc.utils.service

import org.validoc.utils.caching.CachingInfoAndOps
import org.validoc.utils.functions.Monoid
import org.validoc.utils.profiling.ProfileInfo
import org.validoc.utils.retry.RetryInfo

import scala.language.higherKinds
import scala.reflect.ClassTag
import org.validoc.utils.reflection.ClassTags._

case class ServicesSummary[M[_]](endPoints: Seq[EndPointInfo[M]],
                                 debugEndPoints: Seq[DebugEndPointInfo],
                                 cachedServices: Seq[CachingInfoAndOps],
                                 profileServices: Seq[ProfileInfo],
                                 retryServices: Seq[RetryInfo]
                                )

object ServicesSummary {
  def apply[M[_]](sds: Seq[AbstractServiceDescription[M, _, _]]): ServicesSummary[M] = {
    val allServices = sds.flatMap(_.fold[List[AbstractServiceDescription[M, _, _]]]((sd, depth) => List(sd), 0).map(_.service))
    val debugEndPoints = allServices.collectAs[DebugEndPointInfo]
    val endPoints = allServices.collectAs[EndPointInfo[M]]
    val cachedServices = allServices.collectAs[CachingInfoAndOps]
    val profiledServices = allServices.collectAs[ProfileInfo]
    val retryServices = allServices.collectAs[RetryInfo]

    ServicesSummary(endPoints, debugEndPoints, cachedServices, profiledServices, retryServices)
  }
}