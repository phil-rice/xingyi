/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.monad
import one.xingyi.core.functions.EitherString

//I cannot work out how to make this work: the lamba type isn't great. Hence the inheritor
class AbstractEitherMonadCanFailTests(implicit val monad: MonadCanFail[EitherString, String]) extends AbstractMonadCanFailTests[EitherString, String] {
  //  override def getT[T](m: EitherString[T]): T = m.right.getOrElse(fail("didn't hold right"))
  override def liftA[T](t: T): EitherString[T] = Right(t)
  override def makeFail(s: String): String = s
  override def failToString(f: String): String = f
  override def getT[X](a: EitherString[X]): X = a.right.getOrElse(fail(s"should have held right but was $a"))
}

class EitherMonadCanFailTests extends AbstractEitherMonadCanFailTests()(MonadCanFail.monadCanFailForEither[String])
