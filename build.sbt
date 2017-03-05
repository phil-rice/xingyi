


val versions = new {
  val scala = "2.11.8"
  //  val scala = "2.12.1"
  val finatra = "2.2.0"
  val scalatest = "3.0.1"
  val mockito = "1.10.19"
  val guice = "4.0"
  val circeVersion = "0.7.0"
  val play = "2.5.12"
}
lazy val commonSettings = Seq(
  version := "1.0",
  organization := "org.validoc",
  publishMavenStyle := true,
  scalaVersion := versions.scala,
  libraryDependencies += "org.mockito" % "mockito-all" % versions.mockito % "test",
  libraryDependencies += "org.scalatest" %% "scalatest" % versions.scalatest % "test"
)

lazy val finatraSettings = commonSettings ++ Seq(

  libraryDependencies += "com.twitter" %% "finatra-http" % versions.finatra,
  libraryDependencies += "com.twitter" %% "finatra-http" % versions.finatra % "test",
  libraryDependencies += "com.twitter" %% "inject-server" % versions.finatra % "test",
  libraryDependencies += "com.twitter" %% "inject-app" % versions.finatra % "test",
  libraryDependencies += "com.twitter" %% "inject-core" % versions.finatra % "test",
  libraryDependencies += "com.twitter" %% "inject-modules" % versions.finatra % "test",
  libraryDependencies += "com.google.inject.extensions" % "guice-testlib" % versions.guice % "test",
  libraryDependencies += "com.twitter" %% "finatra-jackson" % versions.finatra % "test",

  libraryDependencies += "com.twitter" %% "finatra-http" % versions.finatra % "test" classifier "tests",
  libraryDependencies += "com.twitter" %% "inject-server" % versions.finatra % "test" classifier "tests",
  libraryDependencies += "com.twitter" %% "inject-app" % versions.finatra % "test" classifier "tests",
  libraryDependencies += "com.twitter" %% "inject-core" % versions.finatra % "test" classifier "tests",
  libraryDependencies += "com.twitter" %% "inject-modules" % versions.finatra % "test" classifier "tests",
  libraryDependencies += "com.google.inject.extensions" % "guice-testlib" % versions.guice % "test" classifier "tests",
  libraryDependencies += "com.twitter" %% "finatra-jackson" % versions.finatra % "test" classifier "tests"
)
lazy val circeSettings = commonSettings ++ Seq(
  libraryDependencies ++= Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-java8",
    "io.circe" %% "circe-parser",
    "io.circe" %% "circe-generic-extras"
  ).map(_ % versions.circeVersion)
)

lazy val playJsonSetting = commonSettings ++ Seq(
  libraryDependencies += "com.typesafe.play" %% "play-json" % versions.play
)

lazy val core = (project in file("modules/core")).
  settings(commonSettings: _*)

lazy val language = (project in file("modules/language")).
  settings(commonSettings: _*).
  dependsOn(core % "test->test;compile->compile").aggregate(core)

lazy val circe = (project in file("modules/circe")).
  dependsOn(core).aggregate(core).
  settings(circeSettings: _*)

lazy val playJson = (project in file("modules/playJson")).
  dependsOn(core).aggregate(core).
  settings(playJsonSetting: _*)

lazy val finatra = (project in file("modules/finatra")).
  settings(finatraSettings: _*).
  dependsOn(language)

lazy val sample = (project in file("modules/sample")).
  dependsOn(language % "test->test;compile->compile").aggregate(language).dependsOn(playJson).aggregate(playJson).
  settings(commonSettings: _*)

lazy val finatraSample = (project in file("modules/finatraSample")).
  dependsOn(finatra).aggregate(finatra).
  dependsOn(sample).aggregate(sample).
  settings(commonSettings: _*)