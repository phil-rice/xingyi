/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddmustache

import java.io.StringWriter

import com.github.mustachejava.{DefaultMustacheFactory, Mustache => JMustache}
import com.twitter.mustache.ScalaObjectHandler
import one.xingyi.core.json.{JsonMaps, JsonWriter, TemplateEngine}


object Mustache {
  implicit def template[J]: MustacheWithTemplate[J] = Mustache.withTemplate("main.template.mustache") apply("engine.mustache", "Tennis")

  val mf = new DefaultMustacheFactory()
  mf.setObjectHandler(new ScalaObjectHandler)
  def apply(name: String) = new Mustache(mf.compile(name))
  def withTemplate(template: String) = new MustacheBuilder(template)
}

class MustacheBuilder(template: String) {
  def apply[J](name: String, title: String) = new MustacheWithTemplate[J](Mustache(template), Mustache(name), title)
}

case class BodyJsonAndTitle(item: Any, body: String, json: String, title: String)


class MustacheWithTemplate[J](template: Mustache, main: Mustache, title: String) extends TemplateEngine[J] {
  def apply(item: JsonMaps[J]) = {
    val str = main(item)
    template(BodyJsonAndTitle(item, str, item.json, title))
  }
}

class Mustache(mustache: JMustache) {
  def apply(item: Any) = {
    val writer = new StringWriter()
    mustache.execute(writer, item)
    writer.flush()
    writer.toString
  }
}
case class MustacheToHtmlAndJson[J: JsonWriter, T](templateName: String, title: String)(implicit mustacheBuilder: MustacheBuilder) {
  val mf = mustacheBuilder(templateName, title)
  //  override def apply(json: String, t: T) = {mf.apply(t, json)}
}
