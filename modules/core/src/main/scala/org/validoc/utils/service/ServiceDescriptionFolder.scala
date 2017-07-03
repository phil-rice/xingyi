package org.validoc.utils.service

import org.validoc.utils.strings.IndentAndString
import scala.language.higherKinds

trait ServiceDescriptionFolder[M[_], T] {
  def root(initial: T, rootHttpServiceDescription: RootHttpServiceDescription[M, _, _]): T

  def service(initial: T, serviceDescription: DelegateServiceDescription[M, _, _, _, _, _]): T

  def serviceWithParam(initial: T, serviceDescription: ParamDelegateServiceDescription[M, _, _, _, _, _, _]): T

  def serviceWithEnrich(initial: T, serviceDescription: MergingTwoServicesDescription[M, _, _, _, _, _, _, _]): T
}

object ServiceDescriptionFolder {

  class ServiceDescriptionFolderForPrintln[M[_]] extends ServiceDescriptionFolder[M, IndentAndString] {
    override def root(initial: IndentAndString, rootHttpServiceDescription: RootHttpServiceDescription[M, _, _]): IndentAndString =
      initial.addLineAndIndent(rootHttpServiceDescription.description)


    override def service(initial: IndentAndString, serviceDescription: DelegateServiceDescription[M, _, _, _, _, _]): IndentAndString = {
      initial.addLineAndIndent(serviceDescription.description + "()" + serviceDescription.report)
    }

    override def serviceWithParam(initial: IndentAndString, serviceDescription: ParamDelegateServiceDescription[M, _, _, _, _, _, _]): IndentAndString = {
      initial.addLineAndIndent(s"${serviceDescription.description}(${serviceDescription.param}) ${serviceDescription.report}")
    }

    override def serviceWithEnrich(initial: IndentAndString, serviceDescription: MergingTwoServicesDescription[M, _, _, _, _, _,_,_]): IndentAndString = {
      initial.addLineAndIndent(s"${serviceDescription.description}() ${serviceDescription.report}")
    }
  }

  class ServiceDescriptionFolderGetAll[M[_]] extends ServiceDescriptionFolder[M, List[ServiceDescription[M, _, _]]] {
    override def root(initial: List[ServiceDescription[M, _, _]], rootHttpServiceDescription: RootHttpServiceDescription[M, _, _]): List[ServiceDescription[M, _, _]] =
      rootHttpServiceDescription :: initial

    override def service(initial: List[ServiceDescription[M, _, _]], serviceDescription: DelegateServiceDescription[M, _, _, _, _, _]): List[ServiceDescription[M, _, _]] =
      serviceDescription :: initial

    override def serviceWithParam(initial: List[ServiceDescription[M, _, _]], serviceDescription: ParamDelegateServiceDescription[M, _, _, _, _, _, _]): List[ServiceDescription[M, _, _]] =
      serviceDescription :: initial

    override def serviceWithEnrich(initial: List[ServiceDescription[M, _, _]], serviceDescription: MergingTwoServicesDescription[M, _, _, _, _, _,_,_]): List[ServiceDescription[M, _, _]] =
      serviceDescription :: initial
  }

}