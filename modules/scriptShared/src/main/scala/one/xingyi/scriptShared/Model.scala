package one.xingyi.scriptShared

import one.xingyi.core.json.{IXingYiLens, IXingYiShared, IXingYiSharedOps}

import scala.language.higherKinds


trait IPerson extends IXingYiShared
trait IPersonNameOps[L[_, _]] extends IXingYiSharedOps[L, IPerson] {
  def name: L[IPerson, String]
}

trait IPersonAddressListOps[L[_, _]] extends IXingYiSharedOps[L, IPerson] {
  def addressList: L[IPerson, List[IAddress]]
}

@Deprecated
trait IPersonAddressOps[L[_, _]] extends IXingYiSharedOps[L,IPerson] {
  def address: L[IPerson, IAddress]
}

@Deprecated
trait IPersonLine12Ops[L[_, _]] extends IXingYiSharedOps[L,IPerson] {
  def line1: L[IPerson, String]
  def line2: L[IPerson, String]
}

trait IPersonTelephoneOps[L[_, _]] extends IXingYiSharedOps[L, IPerson] {
  def telephoneNumber: L[IPerson, ITelephoneNumber]
}

trait IAddress extends IXingYiShared
trait IAddressOps[L[_, _]] extends IXingYiSharedOps[L, IAddress] {
  def line1: L[IAddress, String]
  def line2: L[IAddress, String]
}

trait ITelephoneNumber extends IXingYiShared
trait ITelephoneNumberOps[L[_, _]] extends IXingYiSharedOps[L, ITelephoneNumber] {
  def number: L[ITelephoneNumber, String]
}

