package one.xingyi.core

import scala.language.higherKinds

package object proveInProduction {
  type TwoServiceMerger[M[_], Req, Res] = (Req, Option[(Req, M[Res])], Option[(Req, M[Res])]) => M[Res]

}
