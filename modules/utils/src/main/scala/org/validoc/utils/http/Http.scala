package org.validoc.utils.http

import java.net.URLEncoder

object Status {
  val Ok = Status(200)
  val NotFound = Status(404)
}

case class Status(code: Int) extends AnyVal

case class Body(s: String) extends AnyVal

case class ContentType(s: String) extends AnyVal


trait UriFragment {
  protected def encode(s: String) = URLEncoder.encode(s, "UTF-8")

  def asUriString: String
}

case class HostName(host: String) extends UriFragment {
  override def asUriString: String = host
}

case class Protocol(protocol: String) extends UriFragment {
  require(protocol.forall(_.isLetter), protocol)

  override def asUriString: String = protocol
}

case class Path(path: String) extends UriFragment {
  require(path.startsWith("/"), s"Path should start with / was ${path}")

  override def asUriString: String = path
}

case class QueryParamName(name: String) extends UriFragment {
  override def asUriString: String = encode(name)
}

case class QueryParamValue(value: String) extends UriFragment {
  override def asUriString: String = encode(value)
}

object QueryParam {
  def apply(tuples: (String, String)*):Seq[QueryParam] = tuples.map { case (n, v) => QueryParam(QueryParamName(n), QueryParamValue(v)) }

  def encoded(params: Seq[QueryParam]) = params match {
    case Nil => ""
    case _ => params.map(_.asUriString).mkString("?", "&", "")
  }
}

case class QueryParam(name: QueryParamName, value: QueryParamValue) extends UriFragment {
  def asUriString = s"${name.asUriString}=${value.asUriString}"
}

case class Uri(protocol: Protocol, host: HostName, path: Path, params: QueryParam*) extends UriFragment {
  override def toString = s"Uri($asUriString)"

  override def asUriString: String = s"${protocol.asUriString}://${host.asUriString}${path.asUriString}${QueryParam.encoded(params)}"
}

