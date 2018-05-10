package one.xingyi.core.logging


case class LoggingRecord(time: Long, level: String, msg: Any, throwable: Option[Throwable])