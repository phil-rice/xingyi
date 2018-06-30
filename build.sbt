import sbt.url


val versions = new {
  val scala = "2.12.2"
  //  val scala = "2.12.1"
  val finatra = "18.2.0"
  val scalatest = "3.0.5"
  val mockito = "1.10.19"
  val guice = "4.0"
  val play = "2.5.12"
  val scalapact = "2.1.3"
  val junit = "4.12"
  val json4s = "3.5.3"
  val mustache = "0.9.5"
}

lazy val commonSettings = Seq(
  version := "0.3",
  organization := "one.xingyi",
  publishMavenStyle := true,
  scalaVersion := versions.scala,
  scalacOptions ++= Seq("-feature"),
  libraryDependencies += "org.mockito" % "mockito-all" % versions.mockito % "test",
  libraryDependencies += "org.scalatest" %% "scalatest" % versions.scalatest % "test"
)

lazy val publishSettings = commonSettings ++ Seq(
  pomIncludeRepository := { _ => false },
  publishMavenStyle := true,
  publishArtifact in Test := false,
  licenses := Seq("BSD-style" -> url("http://www.opensource.org/licenses/bsd-license.php")),
  homepage := Some(url("http://example.com")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/phil-rice/pact-stubber"),
      "scm:git@github.com/phil-rice/pact-stubber.git"
    )
  ),
  developers := List(
    Developer(
      id = "phil",
      name = "Phil Rice",
      email = "phil.rice@iee.org",
      url = url("https://www.linkedin.com/in/phil-rice-53959460")
    )
  ),
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  })

lazy val finatraSettings = publishSettings ++ Seq(
  // https://mvnrepository.com/artifact/org.apache.thrift/libthrift
  //  libraryDependencies += "org.apache.thrift" % "libthrift" % "0.5.0-1",

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

//lazy val playSettings = publishSettings ++ Seq(
//  libraryDependencies ++= Seq(jdbc, ehcache, ws, guice)
//
//)

lazy val pactSettings = publishSettings ++ Seq(
  libraryDependencies += "com.itv" %% "scalapact-scalatest" % versions.scalapact % "test",
  libraryDependencies += "junit" % "junit" % versions.junit % "test"
)

lazy val reflectionSettings = publishSettings ++ Seq(
  libraryDependencies += "org.scala-lang" % "scala-reflect" % versions.scala,
  libraryDependencies += "org.scala-lang" % "scala-compiler" % versions.scala
)

lazy val json4sSettings = publishSettings ++ Seq(
  libraryDependencies += "org.json4s" %% "json4s-native" % versions.json4s
)

lazy val apacheDbcp2Settings = publishSettings ++ Seq(
  libraryDependencies += "org.apache.commons" % "commons-dbcp2" % "2.3.0",
  libraryDependencies += "com.h2database" % "h2" % "1.4.197"
)

lazy val mustacheSettings = publishSettings ++ Seq(
  libraryDependencies += "com.github.spullara.mustache.java" % "scala-extensions-2.11" % versions.mustache
)

lazy val scalatestSettings = publishSettings ++ Seq(
  libraryDependencies += "org.scalatest" %% "scalatest" % versions.scalatest
)

lazy val core = (project in file("modules/core")).
  settings(publishSettings: _*)

lazy val jdbc = (project in file("modules/jdbc")).
  dependsOn(core % "test->test;compile->compile").
  settings(publishSettings: _*)

val apachejdbc = (project in file("modules/apachejdbc")).
  dependsOn(core % "test->test;compile->compile").
  settings(apacheDbcp2Settings)

val cddmustache = (project in file("modules/cddmustache")).
  dependsOn(core % "test->test;compile->compile").
  aggregate(core).
  settings(mustacheSettings)

val cddscalatest = (project in file("modules/cddscalatest")).
  dependsOn(core % "test->test;compile->compile").
  dependsOn(cddengine % "test->test;compile->compile").
  settings(scalatestSettings)


lazy val tagless = (project in file("modules/tagless")).
  settings(publishSettings: _*).
  dependsOn(core % "test->test;compile->compile").aggregate(core)

lazy val cddscenario = (project in file("modules/cddscenario")).
  settings(reflectionSettings: _*).
  dependsOn(core % "test->test;compile->compile").aggregate(core)

lazy val cep = (project in file("modules/cep")).
  settings(reflectionSettings: _*).
  dependsOn(core % "test->test;compile->compile").aggregate(core)

val cddexamples = (project in file("modules/cddexamples")).
  dependsOn(core % "test->test;compile->compile").
  dependsOn(cddengine % "test->test;compile->compile").
  dependsOn(cddscalatest % "test->test;compile->compile").
  dependsOn(json4s % "test->test;compile->compile").
  dependsOn(cddmustache % "test->test;compile->compile").
//  dependsOn(cddlegacy % "test->test;compile->compile").
  dependsOn(apachejdbc % "test->test;compile->compile").
  settings(publishSettings)

lazy val cddengine = (project in file("modules/cddengine")).
  settings(publishSettings: _*).
  dependsOn(core % "test->test;compile->compile").aggregate(core).
  dependsOn(cddscenario % "test->test;compile->compile").aggregate(cddscenario)

lazy val cddscripts = (project in file("modules/cddscripts")).
  settings(publishSettings: _*).
  dependsOn(core % "test->test;compile->compile").aggregate(core)

lazy val test = (project in file("modules/test")).
  settings(publishSettings: _*).
  dependsOn(core % "test->test;compile->compile").
  dependsOn(cep % "test->test;compile->compile").
  dependsOn(apachejdbc % "test->test;compile->compile").
  dependsOn(json4s % "test->test;compile->compile").
  aggregate(core)

lazy val sampleServer = (project in file("modules/sampleServer")).
  settings(publishSettings: _*).
  settings(publishArtifact := false).
  dependsOn(core % "test->test;compile->compile").aggregate(core).
  dependsOn(tagless % "test->test;compile->compile").aggregate(tagless).
  dependsOn(sample % "test->test;compile->compile").aggregate(sample).
  dependsOn(jdbc % "test->test;compile->compile").aggregate(sample).
  dependsOn(json4s)

lazy val finatra = (project in file("modules/finatra")).
  dependsOn(core % "test->test;compile->compile").aggregate(core).
  settings(finatraSettings: _*)

lazy val json4s = (project in file("modules/json4s")).
  dependsOn(core % "test->test;compile->compile").aggregate(core).
  settings(json4sSettings: _*)

lazy val sample = (project in file("modules/sample")).
  dependsOn(core % "test->test;compile->compile").aggregate(core).
  dependsOn(tagless % "test->test;compile->compile").aggregate(tagless).
  settings(publishArtifact := false).
  settings(pactSettings: _*)

lazy val finatraSample = (project in file("modules/finatraSample")).
  dependsOn(core % "test->test;compile->compile").aggregate(core).
  dependsOn(finatra % "test->test;compile->compile").aggregate(finatra).
  dependsOn(sample % "test->test;compile->compile").aggregate(sample).
  dependsOn(json4s).
  settings(publishArtifact := false).
  settings(publishSettings: _*)

val root = (project in file(".")).
  settings(publishSettings).
  settings(publishArtifact := false).
  aggregate(
    apachejdbc,//
    cddscenario,//
    cddengine,//
    cddmustache,//
    cddscalatest,//
    cep,
    core,//
    finatra,//
    finatraSample,
    sample,
    sampleServer,
    json4s,//
    tagless//
  )
