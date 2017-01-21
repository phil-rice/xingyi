name := "phil-utils"

version := "1.0"

scalaVersion := "2.12.1"


val versions = new {
  val scalatest = "3.0.1"
  val mockito = "1.10.19"
}

libraryDependencies += "org.mockito" % "mockito-all" % versions.mockito % "test"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"