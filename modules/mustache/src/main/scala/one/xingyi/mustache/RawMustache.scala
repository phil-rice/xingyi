/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.mustache
import com.github.mustachejava.{DefaultMustacheFactory, Mustache => JMustache}
import one.xingyi.core.json.{JsonMaps, JsonWriter, TemplateEngine}
import one.xingyi.core.strings.Strings
import one.xingyi.mustache.RawMustache.mf

trait NameToMustacheTemplate extends (String => RawMustache)
object NameToMustacheTemplate {
  implicit def defaultNameToMustacheTemplate: NameToMustacheTemplate = { name => new RawMustache(mf.compile(name)) }
}
case class BodyJsonAndTitle(item: Any, body: String, json: String, title: String)

case class Mustache[J: JsonWriter](title: String, templateName: String = "engine.mustache", mainTemplateName: String = "main.template.mustache")(implicit nameToMustacheTemplate: NameToMustacheTemplate) extends TemplateEngine[J] {
  lazy val main = nameToMustacheTemplate(mainTemplateName)
  lazy val template = nameToMustacheTemplate(templateName)
  def apply(item: JsonMaps[J]) = main(BodyJsonAndTitle(item.map, template(item.map), item.json, title))
}

object RawMustache {
  val mf = new DefaultMustacheFactory()
  mf.setObjectHandler(new ScalaObjectHandler)
  def apply(name: String) = new RawMustache(mf.compile(name))
}

class RawMustache(mustache: JMustache) {
  def apply(item: Any): String = Strings.useStringWriter(writer => mustache.execute(writer, item))
}

