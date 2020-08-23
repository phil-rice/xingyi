/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.scriptModel1

import one.xingyi.core.json.{IXingYiLens, IXingYiShared, IXingYiSharedOps}
import one.xingyi.core.script.XingYiInterface

import scala.language.higherKinds


trait IPerson extends IXingYiShared

@XingYiInterface(clazzes = Array(classOf[IPerson]))
trait IPersonNameOps[L[_, _], P <: IPerson] extends IXingYiSharedOps[L, P] {
  def nameLens: L[P, String]
}

@XingYiInterface(clazzes = Array(classOf[IPerson]))
trait IPersonLine12Ops[L[_, _], P <: IPerson] extends IXingYiSharedOps[L, P] {
  def line1Lens: L[P, String]
  def line2Lens: L[P, String]
}

@XingYiInterface(clazzes = Array(classOf[IPerson], classOf[ITelephoneNumber]))
trait IPersonTelephoneOps[L[_, _], P <: IPerson, T <: ITelephoneNumber] extends IXingYiSharedOps[L, P] {
  def telephoneNumberLens: L[P, T]
}

trait ITelephoneNumber extends IXingYiShared

@XingYiInterface(clazzes = Array( classOf[ITelephoneNumber]))
trait ITelephoneNumberOps[L[_, _], T <: ITelephoneNumber] extends IXingYiSharedOps[L, T] {
  def numberLens: L[T, String]
}


