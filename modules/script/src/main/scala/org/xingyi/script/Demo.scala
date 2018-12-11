package org.xingyi.script
import javax.script.{Invocable, ScriptEngineManager}



object Demo extends App {
  val manager = new ScriptEngineManager
  val engine = manager.getEngineByName("JavaScript")

  // JavaScript code in a String
  val script = "function hello(name) { return 'Hello, ' + name; }"
  // evaluate script
  engine.eval(script)

  // javax.script.Invocable is an optional interface.
  // Check whether your script engine implements or not!
  // Note that the JavaScript engine implements Invocable interface.
  val inv = engine.asInstanceOf[Invocable]

  // invoke the global function named "hello"
  val result = inv.invokeFunction("hello", "Scripting!!")
println(result)
}
