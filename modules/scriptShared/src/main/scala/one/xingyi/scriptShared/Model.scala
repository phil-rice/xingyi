package one.xingyi.scriptShared

import one.xingyi.core.json.{IXingYiLens, IXingYiShared, IXingYiSharedOps}
import one.xingyi.core.script.XingYiInterface

import scala.language.higherKinds


trait IPerson extends IXingYiShared
@XingYiInterface(clazzes = Array(classOf[IPerson]))
trait IPersonNameOps[L[_, _], P <: IPerson] extends IXingYiSharedOps[L, P] {
  def nameLens: L[P, String]
}

@XingYiInterface(clazzes = Array(classOf[IPerson], classOf[IAddress]))
trait IPersonAddressListOps[L[_, _], P <: IPerson, A <: IAddress] extends IXingYiSharedOps[L, P] {
  def addressListLens: L[P, List[A]]
}


@XingYiInterface(clazzes = Array(classOf[IPerson], classOf[ITelephoneNumber]))
trait IPersonTelephoneOps[L[_, _], P <: IPerson, T <: ITelephoneNumber] extends IXingYiSharedOps[L, P] {
  def telephoneNumberLens: L[P, T]
}

trait IAddress extends IXingYiShared
@XingYiInterface(clazzes = Array( classOf[IAddress]))
trait IAddressOps[L[_, _], A <: IAddress] extends IXingYiSharedOps[L, A] {
  def line1Lens: L[A, String]
  def line2Lens: L[A, String]
}

trait ITelephoneNumber extends IXingYiShared
@XingYiInterface(clazzes = Array( classOf[ITelephoneNumber]))
trait ITelephoneNumberOps[L[_, _], T <: ITelephoneNumber] extends IXingYiSharedOps[L, T] {
  def numberLens: L[T, String]
}


@Deprecated
@XingYiInterface(clazzes = Array(classOf[IPerson], classOf[IAddress]))
trait IPersonAddressOps[L[_, _], P <: IPerson, A <: IAddress] extends IXingYiSharedOps[L, P] {
  def addressLens: L[P, A]
}

@Deprecated
@XingYiInterface(clazzes = Array(classOf[IPerson]))
trait IPersonLine12Ops[L[_, _], P <: IPerson] extends IXingYiSharedOps[L, P] {
  def line1Lens: L[P, String]
  def line2Lens: L[P, String]
}
