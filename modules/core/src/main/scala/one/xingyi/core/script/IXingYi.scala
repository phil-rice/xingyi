/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.script

import javax.script.{Invocable, ScriptEngine}
import jdk.nashorn.api.scripting.{ScriptObjectMirror, ScriptUtils}
import one.xingyi.core.builder.HasId
import one.xingyi.core.http._
import one.xingyi.core.json._
import one.xingyi.core.optics.Lens

import scala.language.postfixOps


trait IXingYiLoader extends (String => IXingYi)

object IXingYiLoader {
  implicit def defaultLoader: IXingYiLoader = new DefaultXingYiLoader()
}

class DefaultXingYiLoader() extends IXingYiLoader {
  override def apply(javaScript: String): IXingYi = {
    import jdk.nashorn.api.scripting.NashornScriptEngineFactory
    val engine: ScriptEngine = new NashornScriptEngineFactory().getScriptEngine("--language=es6 ")
    engine.eval(javaScript)
    new DefaultXingYi(engine)
  }
}

trait DomainMaker[T <: Domain] {
  def create(mirror: Object): T
}

trait Domain {
  def mirror: Object
}

trait ServerDomain {
  def lens: List[String]

  val lensString = lens.mkString(",")

  val contentType: String = s"application/xingyi.$lensString"

}

trait EntityPrefix[T] {
  def apply(): String
}
case class LinkDetail(verb: String, urlPattern: String)

trait Links[T] extends (T => List[LinkDetail])

case class ServerPayload[SharedE, DomainE](status: Status, domainObject: DomainE, domain: DomainDetails[SharedE, DomainE])(implicit val links: Links[DomainE])

trait ToContentType[Req] {
  def apply(req: Req): String
}

trait HasHost[T] {
  def apply(t: T): String
}

object ServerPayload extends JsonWriterLanguage {
  implicit def toServerResponse[J, Req, SharedE, DomainE]
  (implicit jsonWriter: JsonWriter[J], entityPrefix: EntityPrefix[DomainE],
   hasId: HasId[Req, String], hasHost: HasHost[Req],
   projection: Projection[SharedE, DomainE], toContentType: ToContentType[Req]): ToServiceResponse[Req, ServerPayload[SharedE, DomainE]] = { req =>
    payload =>
      val host = hasHost(req)
      ServiceResponse(payload.status, Body(
        s"http://$host/${entityPrefix()}/code/" + payload.domain.codeHeader + "\n" +
          jsonWriter(projection.toJson(payload.domainObject) |+| ("_links" ->
            JsonObject(payload.links(payload.domainObject).map {
              case LinkDetail(verb, pattern) => verb -> JsonString(pattern.replace("<id>", hasId(req)).replace("<host>", host))
            }: _*)))),
        List(Header("content-type", toContentType(req)), Header("xingyi", payload.domain.codeHeader)))
  }
}


trait IXingYi {
  def parse[T <: Domain](s: String)(implicit domainMaker: DomainMaker[T]): T

  protected def rawRender(name: String, t: Object): String


  def stringLens[T <: Domain](name: String)(implicit maker: DomainMaker[T]): Lens[T, String]

  def objectLens[T1 <: Domain, T2 <: Domain](name: String)(implicit maker1: DomainMaker[T1], maker2: DomainMaker[T2]): Lens[T1, T2]

  def listLens[T1 <: Domain, T2 <: Domain](name: String)(implicit maker1: DomainMaker[T1], maker2: DomainMaker[T2]): Lens[T1, List[T2]]

  def render(name: String, t: Domain): String = rawRender(name, t.mirror)
}

class XingYiExecutionException(msg: String, cause: Throwable) extends RuntimeException(msg, cause)

class DefaultXingYi(engine: ScriptEngine) extends IXingYi {
  val inv = engine.asInstanceOf[Invocable]

  def wrap[X](name: => String, fn: => X): X = try {
    fn
  } catch {
    case e: Exception => throw new XingYiExecutionException(s"Error executing $name", e)
  }

  override def rawRender(name: String, t: Object): String = wrap("render", inv.invokeFunction(s"render_$name", t).asInstanceOf[String])

  override def objectLens[T1 <: Domain, T2 <: Domain](name: String)(implicit maker1: DomainMaker[T1], maker2: DomainMaker[T2]): Lens[T1, T2] = Lens[T1, T2](
    { t => wrap(s"objectLens.get($name)", maker2.create(inv.invokeFunction("getL", name, t.mirror))) },
    { (t, v) => wrap(s"objectLens.set($name)", maker1.create(inv.invokeFunction("setL", name, t.mirror, v.mirror))) })

  override def stringLens[T <: Domain](name: String)(implicit maker: DomainMaker[T]): Lens[T, String] = Lens[T, String](
    { t => wrap(s"stringLens.get($name)", inv.invokeFunction("getL", name, t.mirror).asInstanceOf[String]) },
    { (t, v) => wrap(s"objectstringLensLens.set($name)", maker.create(inv.invokeFunction("setL", name, t.mirror, v))) })

  def parse[T <: Domain](s: String)(implicit domainMaker: DomainMaker[T]) = wrap("parse", domainMaker.create(inv.invokeFunction("parse", s)))


  import jdk.nashorn.api.scripting.ScriptObjectMirror
  import java.util

  def toList(original: Any): List[Object] = {
    if (!original.isInstanceOf[ScriptObjectMirror]) throw new IllegalStateException("This is supposed to be an object mirror and it isn't" + original.getClass + " / " + original)
    val jsOriginal = original.asInstanceOf[ScriptObjectMirror]
    if (!jsOriginal.isArray) throw new IllegalStateException("This is supposed to be an array mirror and it isn't" + original.getClass + " / " + original)
    val listResult = new util.ArrayList[Object]
    val length = jsOriginal.get("length").asInstanceOf[Integer]
    (0 to length - 1) map (i => jsOriginal.get("" + i)) toList
  }

  //  def fromMirror(d: Domain) = d.mirror.asInstanceOf[ScriptObjectMirror].
  def fromList(list: List[Domain]): Object =
    inv.invokeFunction("makeArray", list.map(_.mirror): _*)


  override def listLens[T1 <: Domain, T2 <: Domain](name: String)(implicit maker1: DomainMaker[T1], maker2: DomainMaker[T2]): Lens[T1, List[T2]] = Lens[T1, List[T2]](
    { t => toList(inv.invokeFunction("getL", name, t.mirror)).map(maker2.create) }, {
      (t, v) => maker1.create(inv.invokeFunction("setL", name, t.mirror, fromList(v)))
    })
}


