package one.xingyi.core

package object optics {
  type Validator[T, Issue] = List[String] => T => List[Issue]

}
