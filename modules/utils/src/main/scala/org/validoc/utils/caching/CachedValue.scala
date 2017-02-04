package org.validoc.utils.caching

case class CachedId(id: Long)

case class CachedValue[M[_], T](time: Long, inFlightId: CachedId, inFlight: Option[M[T]], value: Option[M[T]]) {
  def valueToUse: M[T] = value.getOrElse(inFlight.getOrElse(throw new RuntimeException("Should not get this. trying to return future from CachedValue")))

}
