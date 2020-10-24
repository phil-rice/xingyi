package one.xingyi.core.accessors

trait HasChildren[Main, Children] extends (Main => Seq[Children])

