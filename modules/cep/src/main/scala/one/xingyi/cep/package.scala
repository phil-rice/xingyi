package one.xingyi

package object cep {

  type StringMap = Map[String, String]
  type ValueFn = StringMap => String
  type WhereFn = StringMap => Boolean
}
