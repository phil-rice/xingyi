package one.xingyi.core

import scala.language.higherKinds

package object databaseService {

  type SPKleisli[M[_], Req <: DatabaseRequest, Res] = Req => M[Res]
}
