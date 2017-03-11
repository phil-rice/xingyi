package org.validoc.utils.logging

import java.util.concurrent.atomic.AtomicInteger

import org.validoc.utils.concurrency.Async
import org.validoc.utils.time.{NanoTimeService, SystemClockNanoTimeService}
import scala.language.higherKinds
import scala.util.{Failure, Success}


object LoggingMemoriser extends LoggingMemoriser {

  val key = "LoggingMemoriserId"

  override def nanoTimeService: NanoTimeService = SystemClockNanoTimeService

  private val nextId = new AtomicInteger()

  def nextTraceId = nextId.incrementAndGet().toString

}


trait LoggingMemoriser  {

  def nanoTimeService: NanoTimeService

  val map = scala.collection.concurrent.TrieMap[String, LoggingRecords]()

  val secretMap = scala.collection.concurrent.TrieMap[String, String]()

  def registerSecret(secret: String, replacedmentFn: String => String) = secretMap.put(secret, replacedmentFn(secret))

  def removeSecrets(msg: String) = secretMap.foldLeft(msg) { case (acc, (secret, replacement)) => acc.replaceAllLiterally(secret, replacement) }

  def addSpan(purpose: String, startId: SpanId, midTime: Long, endId: SpanId, result: Any)(implicit loggingAdapter: LoggingAdapter): Unit = useTraceId { lr =>
    val startTime = lr.records(startId.id).time
    val startMessage = lr.records(startId.id).msg.toString
    val endTime = lr.records(endId.id).time
    val endMessage = lr.records(endId.id).msg.toString
    lr.copy(spans = lr.spans :+ Span(purpose, startId, startTime, startMessage, midTime, endId, endTime, endMessage, result.toString))
  }

  def traceNow[X](block: => X)(implicit loggingAdapter: LoggingAdapter): LoggingReport[X] = {
    val traceId = setup
    try {
      val result = block
      LoggingReport(Success(result), map(traceId))
    } catch {
      case e: Exception => LoggingReport(Failure(e), map(traceId))
    } finally {
      cleanup
    }
  }

  import Async._

  def traceFuture[M[_] : Async, X](block: => M[X])(implicit loggingAdapter: LoggingAdapter): M[LoggingReport[X]] = {
    val traceId: String = setup
    block.transform(tryResult => LoggingReport[X](tryResult, map(traceId)).lift).registerSideEffectWhenComplete(_ => cleanup)
  }


  private def setup[X](implicit loggingAdapter: LoggingAdapter): String = {
    val traceId = LoggingMemoriser.nextTraceId
    loggingAdapter.setMDCvalue(LoggingMemoriser.key, traceId)
    map.put(traceId.toString, LoggingRecords(Vector(), Vector()))
    traceId
  }

  private def cleanup(implicit loggingAdapter: LoggingAdapter): Unit = {
    thisTraceId.fold()(traceId => map.remove(traceId))
    loggingAdapter.removeMDCvalue(LoggingMemoriser.key)
  }

  def thisTraceId(implicit loggingAdapter: LoggingAdapter): Option[String] = {
    loggingAdapter.getMDCvalue(LoggingMemoriser.key)
  }

  private val lock = new Object

  def memorise(level: LogLevel, msg: Any, throwable: Throwable)(implicit loggingAdapter: LoggingAdapter): SpanId = {
    useTraceId(lr => lr.copy(records = lr.records :+ LoggingRecord(nanoTimeService(), level, msg, Option(throwable))))
  }

  def useTraceId(fn: LoggingRecords => LoggingRecords)(implicit loggingAdapter: LoggingAdapter): SpanId ={
    lock.synchronized {
      thisTraceId match {
        case Some(traceId) =>
          map.get(traceId) match {
            case Some(lr@LoggingRecords(vector, spans)) =>
              map.put(traceId, fn(lr))
              SpanId(vector.size)
            case _ => throw new RuntimeException(s"Cannot find traceId $traceId in the map. Legal values are ${map.keys.toList.sorted} map is $map")
          }
        case _ => SpanId(0)
      }
    }}

  override def toString: String = s"${getClass.getSimpleName}($map)"
}
