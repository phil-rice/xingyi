package org.validoc.utils.http

import java.net.URLEncoder

object Status{
  val Ok = Status(200)
  val NotFound = Status(404)
}
case class Status(code: Int) extends AnyVal

case class Body(s: String) extends AnyVal

case class ContentType(s: String) extends AnyVal

case class AcceptHeader(s: String) extends AnyVal

case class Header(name: String, value: String)


trait UriFragment {
  protected def encode(s: String) = URLEncoder.encode(s, "UTF-8")

  def encoded: String
}

case class HostName(host: String) extends UriFragment {
  override def encoded: String = encode(host)
}

case class Protocol(protocol: String) extends UriFragment {
  override def encoded: String = encode(protocol)
}

case class Path(path: String) extends UriFragment {
  require(path.startsWith("/"), s"Path should start with / was ${path}")

  override def encoded: String = encode(path)
}

case class QueryParamName(name: String) extends UriFragment {
  override def encoded: String = encode(name)
}

case class QueryParamValue(value: String) extends UriFragment {
  override def encoded: String = encode(value)
}

object QueryParam {
  def encoded(params: Seq[QueryParam]) = params match {
    case Nil => ""
    case _ => params.map(_.encoded).mkString("?", "&", "")
  }
}

case class QueryParam(name: QueryParamName, value: QueryParamValue) extends UriFragment {
  def encoded = s"${URLEncoder.encode(name.name, "UTF-8")}=${URLEncoder.encode(value.value, "UTF-8")}"
}

case class Uri(protocol: Protocol, host: HostName, path: Path, params: QueryParam*) extends UriFragment {
  override def toString = s"Uri($encoded)"

  override def encoded: String = s"${protocol.encoded}://${host.encoded}${path.encoded}${QueryParam.encoded(params)}"
}

