package one.xingyi.cep.exceptions
import one.xingyi.cep.LastEventAndData
import one.xingyi.cep.model.{Event, StringField}


class CannotgetData(stringField: StringField, lastEventAndData: LastEventAndData, name: String, event: Event, cause: Throwable) extends RuntimeException(
  s"""
     |stringField: $stringField
     |name:  $name
     |event: $event
     |Data:
     |$lastEventAndData
  """.stripMargin, cause)
