package org.validoc.utils.json

import org.validoc.utils.parser.Parser

trait ToJson[T] extends (T => String)

trait FromJson[T] extends Parser[T]
