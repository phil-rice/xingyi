/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.http

import java.net.{URL, URLEncoder}

object Method {
  def apply(s: String) = s.toLowerCase match {
    case "get" => Get
    case "post" => Post
    case "put" => Put
    case "delete" => Delete
  }
}
sealed trait Method {
  override def toString: String = getClass.getSimpleName.dropRight(1)

}
case object Get extends Method
case object Post extends Method
case object Put extends Method
case object Delete extends Method

object Status {
  val Ok = Status(200)
  val NotFound = Status(404)
}

case class Status(code: Int) extends AnyVal
case class Body(s: String) extends AnyVal
trait Header{
  def name: String
  def value: String
}
object Header{
  def apply(name: String, value: String): Header = SimpleHeader(name, value)
}
abstract class SpecificHeader(val name: String) extends Header
case class ContentType(value: String) extends SpecificHeader(Headers.contentType)
case class AcceptHeader(value: String) extends SpecificHeader(Headers.accept)

case class SimpleHeader(name: String, value: String) extends Header


trait UriFragment {
  protected def encode(s: String) = URLEncoder.encode(s, "UTF-8")
  def asUriString: String
}

case class ServiceName(name: String) extends AnyVal

case class ProtocolHostAndPort(protocol: Protocol, hostName: HostName, port: Port)


case class HostName(host: String) extends UriFragment {
  override def asUriString: String = host
}


case class Protocol(protocol: String) extends UriFragment {
  require(protocol.forall(_.isLetter), protocol)

  override def asUriString: String = protocol
}

case class Port(port: Int) extends UriFragment {
  override def asUriString: String = port.toString
}

case class Path(path: String) extends UriFragment {
  require(path.startsWith("/") || path == "", s"Path should start with / was ${path}")

  override def asUriString: String = path
}

case class QueryParamName(name: String) extends UriFragment {
  override def asUriString: String = encode(name)
}

case class QueryParamValue(value: String) extends UriFragment {
  override def asUriString: String = encode(value)
}

class QueryParamException(msg: String) extends Exception(msg)

object QueryParam {
  def apply(paramString: String): Seq[QueryParam] = {
    val withoutQuestionMark = if (paramString.startsWith("?")) paramString.substring(1) else paramString
    withoutQuestionMark match {
      case "" => Seq()
      case s =>
        val parts = s.split("&")
        parts.toSeq.map {
          part =>
            val nameValue = part.split("=")
            if (nameValue.size != 2) throw new QueryParamException(s"QueryParam part must have one and only one equals in it. This part is [${nameValue.toList}] from url $s")
            QueryParam(QueryParamName(nameValue(0)), QueryParamValue(nameValue(1)))
        }
    }
  }

  def apply(tuples: (String, String)*): Seq[QueryParam] = tuples.map { case (n, v) => QueryParam(QueryParamName(n), QueryParamValue(v)) }

  def encoded(params: Seq[QueryParam]) = params match {
    case Nil => ""
    case _ => params.map(_.asUriString).mkString("?", "&", "")
  }
}

case class QueryParam(name: QueryParamName, value: QueryParamValue) extends UriFragment {
  def asUriString = s"${name.asUriString}=${value.asUriString}"
}

class ProtocolException(msg: String) extends Exception(msg)

object Uri {
  def apply(s: String): Uri = {
    if (s.contains("//")) {
      val url = new URL(s)
      val port = url.getPort match {
        case -1 if url.getProtocol == "http" => 80
        case -1 if url.getProtocol == "https" => 443
        case -1 => throw new ProtocolException("A port must be specified if the protocol isn't http or https")
        case x => x
      }

      def str(x: String) = if (x == null) "" else x

      Uri(Some(Domain(Protocol(url.getProtocol), HostName(url.getHost), Port(port))), Path(str(url.getPath)), QueryParam(str(url.getQuery)): _*)
    } else {
      s.split("\\?") match {
        case Array(path) => Uri(None, Path(path))
        case Array(path, queries) => Uri(None, Path(path), QueryParam(queries): _*)

      }
    }
  }
}

case class Domain(protocol: Protocol, host: HostName, port: Port) {
  def asUriString: String =
    s"${protocol.asUriString}://${host.asUriString}:${port.port}"
}

case class Uri(domain: Option[Domain], path: Path, params: QueryParam*) extends UriFragment {
  override def toString = s"Uri($asUriString)"

  override def asUriString: String = {
    val domainStr = domain.fold("")(_.asUriString)
    s"${domainStr}${path.asUriString}${QueryParam.encoded(params)}"
  }
}

