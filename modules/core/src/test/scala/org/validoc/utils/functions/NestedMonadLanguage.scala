package org.validoc.utils.functions

import org.validoc.utils.monads.Monad

trait NestedMonadLanguage {

  import org.validoc.utils.monads.MonadLibrary._

  implicit class NestedValuePimper[M1[_], M2[_], T](t: M1[M2[T]])(implicit m1: Monad[M1], m2: Monad[M2]) {
    def ~~>[T1](fn: T => T1): M1[M2[T1]] = t.map(_.map(fn))

    def aggregate[T2](t2: M1[M2[T2]]): (M1[M2[T]], M1[M2[T2]]) = (t, t2)
  }

  def agg[M1[_], M2[_], T1, T2](t1: M1[M2[T1]], t2: M1[M2[T2]])(implicit m1: Monad[M1], m2: Monad[M2]): (M1[M2[T1]], M1[M2[T2]]) = (t1, t2)

  def merge[M1[_], M2[_], T1, T2, T3](t1: M1[M2[T1]], t2: M1[M2[T2]], fn: (T1, T2) => T3)(implicit m1: Monad[M1], m2: Monad[M2]): M1[M2[T3]] = {
    t1.flatMap[M2[T3]] { m2t1 =>
      t2.flatMap[M2[T3]] { m2t2 =>
        m1.lift(m2t1.flatMap[T3] { t1 =>
          m2t2.map[T3] { t2 => fn(t1, t2) }
        })
      }
    }
  }

  implicit class NestedMonadPimper[M1[_], M2[_], T1, T2](tuple: (M1[M2[T1]], M1[M2[T2]]))(implicit m1: Monad[M1], m2: Monad[M2]) {
    def merge[T3](fn: (T1, T2) => T3): M1[M2[T3]] = NestedMonadLanguage.this.merge[M1, M2, T1, T2, T3](tuple._1, tuple._2, fn)

  }


}
