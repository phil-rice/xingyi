package one.xingyi.core.streams

import one.xingyi.core.functions.Monoid
import one.xingyi.core.language.Language.MonadWithExceptionSeqPimper
import one.xingyi.core.monad.{Async, MonadWithException, SuccessOrFail}

import scala.language.higherKinds

object StreamLanguage extends StreamLanguage
trait StreamLanguage {
  implicit class StreamPimper[In](s: Stream[In]) {
    def batchMapOverKleislis[M[_], S[_] : SuccessOrFail, Out](batchSize: Int, fn: In => M[Out])(implicit a: Async[M], m: MonadWithException[M]): Stream[S[Out]] = {
      def realMapFn(in: In): M[Out] = try {fn(in) } catch {case e: Exception => m.exception(e)}
      s.grouped(batchSize).map(batch => a.await(batch.map(realMapFn).flattenToSuccessFail[S])).toStream.flatMap(_.toStream)
      //For performance we batch up and block until each block is done.
      //Then we turn into a stream of Outs.
    }
    def sideeffect(fn: In => Unit): Stream[In] = s.map { inp => fn(inp); inp }
    def addAll[Out](fn: In => Out)(implicit monoid: Monoid[Out]): Out =
      s.foldLeft(monoid.zero)((acc, v) => monoid.add(acc, fn(v)))

  }


}
