package org.validoc.utils.exceptions

import org.validoc.utils.functions.MonadWithException
import scala.language.higherKinds

object Exceptions {
  def apply[M[_], T](m: => M[T])(implicit monadWithException: MonadWithException[M]): M[T] =
    try {
      m
    } catch {
      case e: Throwable => monadWithException.exception(e)
    }
}
