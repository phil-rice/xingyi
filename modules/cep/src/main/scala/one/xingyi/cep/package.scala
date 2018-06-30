package one.xingyi

package object cep {

  type StringMap = Map[StringField, String]
  type ValueFn = StringMap => String
  type WhereFn = StringMap => Boolean
}
