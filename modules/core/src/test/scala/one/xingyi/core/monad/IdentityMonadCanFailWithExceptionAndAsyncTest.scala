package one.xingyi.core.monad

import one.xingyi.core.monad.AsyncForScalaFuture.ImplicitsForTest._
import one.xingyi.core.monad.AsyncForScalaFuture._
import org.scalatest.FlatSpecLike

import scala.concurrent.Future

class IdentityMonadCanFailWithExceptionAndAsyncAndStateTest extends AbstractMonadCanFailWithExceptionAndStateTests[IdentityMonad] with FlatSpecLike with AbstractAsyncTests[IdentityMonad] {
  override def async = IdentityMonad.MonadForIdentityMonad
  override def monad = IdentityMonad.MonadForIdentityMonad
  override implicit def monadWithState: MonadWithState[IdentityMonad] = IdentityMonad.MonadForIdentityMonad
  override def isActuallyAsync: Boolean = false
}
