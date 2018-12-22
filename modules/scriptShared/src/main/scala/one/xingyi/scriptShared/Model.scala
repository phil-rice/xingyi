package one.xingyi.scriptShared

import one.xingyi.core.json.{IXingYiLens, IXingYiShared, IXingYiSharedOps}

import scala.language.higherKinds


trait IPerson extends IXingYiShared
trait IPersonNameOps[L[_, _], P <: IPerson] extends IXingYiSharedOps[L, P] {
  def name: L[P, String]
}

trait IPersonAddressListOps[L[_, _], P <: IPerson, A <: IAddress] extends IXingYiSharedOps[L, P] {
  def addressList: L[P, List[A]]
}


trait IPersonTelephoneOps[L[_, _], P <: IPerson, T<:ITelephoneNumber] extends IXingYiSharedOps[L, P] {
  def telephoneNumber: L[P, T]
}

trait IAddress extends IXingYiShared
trait IAddressOps[L[_, _], A <: IAddress] extends IXingYiSharedOps[L, A] {
  def line1: L[A, String]
  def line2: L[A, String]
}

trait ITelephoneNumber extends IXingYiShared
trait ITelephoneNumberOps[L[_, _], T <: ITelephoneNumber] extends IXingYiSharedOps[L, T] {
  def number: L[T, String]
}


@Deprecated
trait IPersonAddressOps[L[_, _], P <: IPerson, A <: IAddress] extends IXingYiSharedOps[L, P] {
  def address: L[P, A]
}

@Deprecated
trait IPersonLine12Ops[L[_, _], P <: IPerson] extends IXingYiSharedOps[L, P] {
  def line1: L[P, String]
  def line2: L[P, String]
}
