package one.xingyi.core.functions

object Streams extends Streams
trait Streams {
  def unfold[S, A](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => a #:: unfold(s)(f)
    case None => Stream.empty[A]
  }
  def unfoldList[S, A](z: S)(f: S => Option[(List[A], S)]): Stream[A] = f(z) match {
    case Some((a, s)) => a.toStream #::: unfoldList(s)(f)
    case None => Stream.empty[A]
  }
  def unfoldIndexedList[S, A](n: Int, z: S)(f: (Int, S) => Option[(List[A], S)]): Stream[A] = {
    f(n, z) match {
      case Some((a, s)) => a.toStream #::: unfoldIndexedList[S, A](n + 1, s)(f)
      case None => Stream.empty[A]
    }
  }

}