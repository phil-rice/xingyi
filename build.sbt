


val versions = new {
  val scalatest = "3.0.1"
  val mockito = "1.10.19"
}

lazy val commonSettings = Seq(
  version := "1.0",
  scalaVersion := "2.12.1",
  libraryDependencies += "org.mockito" % "mockito-all" % versions.mockito % "test",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

lazy val utils = (project in file("modules/utils")).
  settings(commonSettings: _*)