package one.xingyi.utils.json

import one.xingyi.utils.parser.Parser

trait ToJson[T] extends (T => String)

trait FromJson[T] extends Parser[T]
