package one.xingyi.scriptShared

import one.xingyi.core.optics.Lens

trait IPerson
trait IAddress
trait ITelephoneNumber

trait IPersonName extends IPerson
trait IPersonLine1 extends Lens[IPerson, String]  // This is a legacy thing from 'model 1' when line 1 was directly under person
trait IPersonLine2 extends Lens[IPerson, String]
trait IPersonAddress extends Lens[IPerson, IAddress]
trait IPersonAddressList extends Lens[IPerson, List[IAddress]]
trait IPersonTelephoneNumber extends Lens[IPerson, ITelephoneNumber]
trait ITelephoneNumberNumber extends Lens[ITelephoneNumber, String]