package org.validoc.utils.json

trait ToJson[T] extends (T => String)

trait FromJson[T] extends (String => T)
