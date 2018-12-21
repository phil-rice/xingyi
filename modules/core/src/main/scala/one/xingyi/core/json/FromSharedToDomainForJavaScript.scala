package one.xingyi.core.json

trait FromSharedToDomainForJavaScript[Shared, Domain] {
  //  def lens: Seq[LensDefn[_,_]]
}

object FromSharedToDomainForJavaScript {
  implicit def workItOutFromProjection[Shared, Domain](tuple: (IXingYiSharedOps[IXingYiLens, Shared], ObjectProjection[Domain]))
                                                      (implicit proof: ProofOfBinding[Shared, Domain]): WorkOutJavascriptLensFromProjects[Shared, Domain] =
    WorkOutJavascriptLensFromProjects(tuple._1, tuple._2)

}

case class XingYiManualPath[A, B](javascript: String)


case class WorkOutJavascriptLensFromProjects[Shared, Domain](ops: IXingYiSharedOps[IXingYiLens, Shared], projection: ObjectProjection[Domain])(implicit proof: ProofOfBinding[Shared, Domain]) extends FromSharedToDomainForJavaScript[Shared, Domain]
case class WorkOutJavascriptLensManually[Shared, Domain](ops: IXingYiSharedOps[IXingYiLens, Shared], IXingYiSharedOps: IXingYiSharedOps[XingYiManualPath, Shared])
                                                        (implicit proof: ProofOfBinding[Shared, Domain]) extends FromSharedToDomainForJavaScript[Shared, Domain]


