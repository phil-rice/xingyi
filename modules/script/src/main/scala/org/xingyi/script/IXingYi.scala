package org.xingyi.script

import javax.script.{Invocable, ScriptEngine}
import one.xingyi.core.optics.Lens


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

trait DomainMaker[T] {
  def create(mirror: Object): T
}

trait Domain {
  def mirror: Object
}

case class Payload(mirror: Object) extends Domain

object Payload {
  implicit def payloadMaker: DomainMaker[Payload] = new DomainMaker[Payload] {
    override def create(mirror: Object): Payload = Payload(mirror)
  }
}

trait IXingYi {
  def parse(s: String): Payload

  protected def rawRender(name: String, t: Object): String


  def stringLens[T <: Domain](name: String)(implicit maker: DomainMaker[T]): Lens[T, String]

  def objectLens[T1 <: Domain, T2 <: Domain](name: String)(implicit maker1: DomainMaker[T1], maker2: DomainMaker[T2]): Lens[T1, T2]

  def render(name: String, t: Domain): String = rawRender(name, t.mirror)
}


class DefaultXingYi(engine: ScriptEngine) extends IXingYi {
  val inv = engine.asInstanceOf[Invocable]

  override def rawRender(name: String, t: Object): String = inv.invokeFunction(s"render_$name", t).asInstanceOf[String]

  override def objectLens[T1 <: Domain, T2 <: Domain](name: String)(implicit maker1: DomainMaker[T1], maker2: DomainMaker[T2]): Lens[T1, T2] = Lens[T1, T2](
    { t => println(s"objectLens get$name " + t); val r = maker2.create(inv.invokeFunction("getL", "lens_" + name, t.mirror)); println("... " + r); r },
    { (t, v) => maker1.create(inv.invokeFunction("setL", "lens_" + name, t.mirror, v.mirror)) })

  override def stringLens[T <: Domain](name: String)(implicit maker: DomainMaker[T]): Lens[T, String] = Lens[T, String](
    { t => println(s"in stringLen$name get " + t); inv.invokeFunction("getL", "lens_" + name, t.mirror).asInstanceOf[String] },
    { (t, v) => println(s"in string lens$name: " + t + " " + v); val r = maker.create(inv.invokeFunction("setL", "lens_" + name, t.mirror, v)); println("... " + r); r })

  override def parse(s: String): Payload = Payload(inv.invokeFunction("parse", s))

}


