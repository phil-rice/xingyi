import sbt._

object Libs {
  val scalaVersion = "2.12.6"

  val `scalatest` = "org.scalatest" %% "scalatest" % "3.0.5" //Apache License 2.0
  val `mockito` = "org.mockito" % "mockito-all" % "1.10.19"
  val `guice` = "com.google.inject.extensions" % "guice-testlib" % "4.0"
  val `junit` = "junit" % "junit" % "4.12"
  val `scalapact-scalatest` = "com.itv" %% "scalapact-scalatest" % "2.3.1"
  val `scala-reflect` = "org.scala-lang" % "scala-reflect" % scalaVersion
  val `scala-compiler` = "org.scala-lang" % "scala-compiler" % scalaVersion
  val `json4s` = "org.json4s" %% "json4s-native" % "3.6.0"
  val `commons-dbcp2` = "org.apache.commons" % "commons-dbcp2" % "2.5.0"
  val `h2` = "com.h2database" % "h2" % "1.4.197"
  val mustache = "com.github.spullara.mustache.java" % "scala-extensions-2.11" % "0.9.5"
}

object FinatraLibs {
  val finatraVersion = "18.8.0"

  val `finatra-http` = "com.twitter" %% "finatra-http" % finatraVersion
  val `inject-server` = "com.twitter" %% "inject-server" % finatraVersion
  val `inject-app` = "com.twitter" %% "inject-app" % finatraVersion
  val `inject-core` = "com.twitter" %% "inject-core" % finatraVersion
  val `inject-modules` = "com.twitter" %% "inject-modules" % finatraVersion
  val `finatra-jackson` = "com.twitter" %% "finatra-jackson" % finatraVersion
}
