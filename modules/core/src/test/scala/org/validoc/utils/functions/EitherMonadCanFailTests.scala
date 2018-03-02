package org.validoc.utils.functions

import scala.util.Try

//I cannot work out how to make this work: the lamba type isn't great. Hence the inheritor
class AbstractEitherMonadCanFailTests(implicit val monad: MonadCanFail[EitherString, String]) extends AbstractMonadCanFailTests[EitherString, String] {
  //  override def getT[T](m: EitherString[T]): T = m.right.getOrElse(fail("didn't hold right"))
  override def liftA[T](t: T): EitherString[T] = Right(t)
  override def makeFail(s: String): String = s
  override def failToString(f: String): String = f
  override def getT[X](a: EitherString[X]): X = a.right.getOrElse(fail(s"should have held right but was $a"))
}

class EitherMonadCanFailTests extends AbstractEitherMonadCanFailTests()(MonadCanFail.monadCanFailForEither[String])
