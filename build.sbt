
val versions = new {
  val scala = "2.11.8"
  //  val scala = "2.12.1"
  val finatra = "2.2.0"
  val scalatest = "3.0.1"
  val mockito = "1.10.19"
  val guice = "4.0"
}

lazy val commonSettings = Seq(
  version := "1.0",
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
lazy val core = (project in file("modules/core")).
  settings(commonSettings: _*)

lazy val language = (project in file("modules/language")).
  settings(commonSettings: _*).
  dependsOn(core).aggregate(core)

lazy val finatra = (project in file("modules/finatra")).
  settings(finatraSettings: _*).
  dependsOn(language)

lazy val sample = (project in file("modules/sample")).
  dependsOn(language).aggregate(language).
  settings(commonSettings: _*)

lazy val finatraSample = (project in file("modules/finatraSample")).
  dependsOn(finatra).aggregate(finatra).
  dependsOn(sample).aggregate(sample).
  settings(commonSettings: _*)