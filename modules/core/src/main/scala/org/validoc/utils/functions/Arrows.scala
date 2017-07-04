package org.validoc.utils.functions

import scala.language.higherKinds

trait Arrows {

  implicit class FunctionPimper[Req, Res](fn: Req => Res) {
    def ~>[Res2](fn2: Res => Res2): (Req) => Res2 = { res: Req => fn2(fn(res)) }
  }


}
