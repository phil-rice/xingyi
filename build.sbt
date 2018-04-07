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
}

lazy val commonSettings = Seq(
  version := "1.0",
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

lazy val playSettings = publishSettings ++ Seq(
  libraryDependencies ++= Seq(jdbc, ehcache, ws, guice)

)

lazy val pactSettings = publishSettings ++ Seq(
  libraryDependencies += "com.itv" %% "scalapact-scalatest" % versions.scalapact % "test",
  libraryDependencies += "junit" % "junit" % "4.12" % "test"
)

lazy val playJsonSetting = publishSettings ++ Seq(
  libraryDependencies += "com.typesafe.play" %% "play-json" % versions.play
)

lazy val caffeineSettings = publishSettings ++ Seq(
  libraryDependencies += "com.github.blemale" %% "scaffeine" % "2.4.0" % "compile"
)

lazy val core = (project in file("modules/core")).
  settings(publishSettings: _*)

lazy val tagless = (project in file("modules/tagless")).
  settings(publishSettings: _*).
  dependsOn(core % "test->test;compile->compile").aggregate(core)

lazy val sampleServer = (project in file("modules/sampleServer")).
  settings(publishSettings: _*).
  dependsOn(core % "test->test;compile->compile").aggregate(core).
  dependsOn(tagless % "test->test;compile->compile").aggregate(tagless).
  dependsOn(sample % "test->test;compile->compile").aggregate(sample)
//  dependsOn(caffeine % "test->test;compile->compile").aggregate(caffeine)


//lazy val caffeine = (project in file("modules/caffeine")).
//  settings(caffeineSettings: _*).
//  dependsOn(core % "test->test;compile->compile").aggregate(core)
//lazy val language = (project in file("modules/language")).
//  settings(commonSettings: _*).
//  dependsOn(core % "test->test;compile->compile").aggregate(core)


//lazy val playJson = (project in file("modules/playJson")).
//  dependsOn(core % "test->test;compile->compile").aggregate(core).
//  settings(playJsonSetting: _*)
//
//lazy val play = (project in file("modules/play")).
//  dependsOn(core % "test->test;compile->compile").aggregate(core).
//  settings(playSettings: _*)

//
//lazy val playSample = (project in file("modules/playSample")).
//  dependsOn(play % "test->test;compile->compile").aggregate(play).
//  settings(playSettings: _*).enablePlugins(PlayScala)

lazy val finatra = (project in file("modules/finatra")).
  dependsOn(core % "test->test;compile->compile").aggregate(core).
  settings(finatraSettings: _*)

lazy val sample = (project in file("modules/sample")).
  dependsOn(core % "test->test;compile->compile").aggregate(core).
  dependsOn(tagless % "test->test;compile->compile").aggregate(tagless).
  settings(pactSettings: _*)

lazy val finatraSample = (project in file("modules/finatraSample")).
  dependsOn(core % "test->test;compile->compile").aggregate(core).
  dependsOn(finatra % "test->test;compile->compile").aggregate(finatra).
  dependsOn(sample % "test->test;compile->compile").aggregate(sample).
  settings(publishSettings: _*)

val root = (project in file(".")).
  settings(publishArtifact := false).
  aggregate(core, finatra, finatraSample, sample, sampleServer, tagless)
