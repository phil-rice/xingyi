package org.validoc.finatra

import com.twitter.finagle.Service
import com.twitter.finagle.http.{Request, Response, Status}
import com.twitter.util.Future


object MockFinatraService {
  def apply(names: String*) = names.foldLeft(Map[String, MockFinatraService]())((acc, name) => acc + (name -> new MockFinatraService(name)))
}

class MockFinatraService(name: String) extends Service[Request, Response] {
  override def apply(request: Request): Future[Response] = {
    val response = Response(Status.Ok)
    response.contentString = name + ":" + request.path
    Future(response)
  }
}
