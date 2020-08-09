package one.xingyi

import scala.language.higherKinds

package object scientist {
  type TwoServiceMerger[M[_], Req, Res] = (Req, Option[(Req, M[Res])], Option[(Req, M[Res])]) => M[Res]

}
