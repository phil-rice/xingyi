package org.xingyi.script

import javax.script.{Invocable, ScriptEngine}
import one.xingyi.core.optics.Lens

import scala.io.Source


trait IXingYiLoader[T] extends (String => IXingYi[T])

object IXingYiLoader {
  implicit def defaultLoader[T <: AnyRef]: IXingYiLoader[T] = new DefaultXingYiLoader[T]()
}

class DefaultXingYiLoader[T <: AnyRef]() extends IXingYiLoader[T] {
  override def apply(hashCodeIgnoreForNow: String): IXingYi[T] = {
    import jdk.nashorn.api.scripting.NashornScriptEngineFactory
    val engine: ScriptEngine = new NashornScriptEngineFactory().getScriptEngine("--language=es6 ")
    val javaScript = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("demo.js")).mkString
    engine.eval(javaScript)
    new DefaultXingYi[T](engine)
  }
}

trait IXingYi[T] {
  def parse(s: String): T

  def render(name: String, t: T): String

  def stringLens(name: String): Lens[T, String]

  def objectLens(name: String): Lens[T, T]
}


class DefaultXingYi[T <: AnyRef](engine: ScriptEngine) extends IXingYi[T] {
  val inv = engine.asInstanceOf[Invocable]

  // invoke the global function named "hello"
  override def render(name: String, t: T): String = inv.invokeFunction(s"render_$name", t).asInstanceOf[String]

  def objectLens(name: String): Lens[T, T] = Lens[T, T](
    t => inv.invokeFunction("getL", "lens_" + name, t).asInstanceOf[T],
    { (t, v) => inv.invokeFunction("setL", "lens_" + name, t, v).asInstanceOf[T] })

  override def parse(s: String): T = inv.invokeFunction("parse", s).asInstanceOf[T]

  override def stringLens(name: String): Lens[T, String] = objectLens(name) andThen Lens.cast[T, String]

}


