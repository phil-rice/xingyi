import sbt.url
import sbtpgp._


lazy val versions = new {
  //  val scala = "2.13.2"
  val scala12 = "2.12.12"
  val scala13 = "2.13.3"
  val scala = scala13
  val finatra = "20.4.1"
  val scalatest = "3.2.2"
  val seleniumTestSelenium = "1.0.0-M2"
  val selenium = "2.45.0"
  val mockito = "1.10.19"
  val guice = "4.0"
  val play = "2.5.12"
  val scalapact = "2.3.17"
  val junit = "4.12"
  val json4s = "3.6.10"
  val mustache = "0.9.6"
  val supportedScalaVersions = List(scala12, scala13)
}

lazy val commonSettings = Seq(
  version := "0.7.6",
  resolvers += Resolver.sonatypeRepo("snapshots"), //for mustache
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
  homepage := Some(url("https://github.com/phil-rice/xingyi.git")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/phil-rice/xingyi"),
      "scm:git@github.com:phil-rice/xingyi.git"
    )
  ),
  developers := List(
    Developer(
      id = "phil",
      name = "Philip Rice",
      email = "phil.rice@validoc.org",
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

lazy val mustacheSettings = publishSettings ++ Seq(
  libraryDependencies += "com.github.spullara.mustache.java" % "compiler" % versions.mustache
)
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

lazy val scalatestSettings = publishSettings ++ Seq(
  libraryDependencies += "org.scalatest" %% "scalatest" % versions.scalatest
)

lazy val xmlSettings = publishSettings ++ Seq(
  libraryDependencies += {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 12)) => "org.scala-lang.modules" %% "scala-xml" % "1.0.6"
      case _ => "org.scala-lang.modules" %% "scala-xml" % "1.3.0"
    }
  })

lazy val seleniumSettings = xmlSettings ++ Seq(
  libraryDependencies += "org.seleniumhq.selenium" % "selenium-java" % versions.selenium % "test",
  libraryDependencies += "org.scalatestplus" %% "scalatestplus-selenium" % versions.seleniumTestSelenium % "test",
  libraryDependencies += "org.seleniumhq.selenium" % "selenium-chrome-driver" % versions.selenium % "test",
)


lazy val normalCrossScala = Seq(crossScalaVersions := versions.supportedScalaVersions)
lazy val justScala12 = Seq(crossScalaVersions := List(versions.scala12))

lazy val core = (project in file("modules/core")).
  settings(normalCrossScala: _*).
  settings(publishSettings: _*).
  settings(Test / publishArtifact := true)


val apachejdbc = (project in file("modules/apachejdbc")).
  dependsOn(core % "test->test;compile->compile").
  settings(normalCrossScala: _*).
  settings(apacheDbcp2Settings)

val scientist = (project in file("modules/scientist")).
  dependsOn(core % "test->test;compile->compile").
  settings(normalCrossScala: _*).
  settings(apacheDbcp2Settings)


val javaServer = (project in file("modules/javaserver")).
  settings(normalCrossScala: _*).
  settings(publishSettings)

lazy val test = (project in file("modules/test")).
  settings(normalCrossScala: _*).
  settings(publishSettings: _*).
  settings(publishArtifact := false, parallelExecution in Test := false).
  dependsOn(core % "test->test;compile->compile").
  dependsOn(apachejdbc % "test->test;compile->compile").
  dependsOn(json4s % "test->test;compile->compile").
  dependsOn(mustache % "test->test;compile->compile").
  aggregate(core)

lazy val sampleServer = (project in file("examples/sampleServer")).
  settings(normalCrossScala: _*).
  settings(publishSettings: _*).
  settings(publishArtifact := false).
  dependsOn(core % "test->test;compile->compile").aggregate(core).
  //  dependsOn(tagless % "test->test;compile->compile").aggregate(tagless).
  dependsOn(sample % "test->test;compile->compile").aggregate(sample).
  dependsOn(json4s)

lazy val mustache = (project in file("modules/mustache")).
  settings(normalCrossScala: _*).
  settings(mustacheSettings).
  dependsOn(core % "test->test;compile->compile").aggregate(core).
  dependsOn(json4s % "test->test").aggregate(core)

lazy val json4s = (project in file("modules/json4s")).
  settings(normalCrossScala: _*).
  dependsOn(core % "test->test;compile->compile").aggregate(core).
  settings(json4sSettings: _*)

lazy val sample = (project in file("examples/sample")).
  settings(normalCrossScala: _*).
  dependsOn(core % "test->test;compile->compile").aggregate(core).
  //  dependsOn(tagless % "test->test;compile->compile").aggregate(tagless).
  //  settings(publishArtifact := false).
  settings(pactSettings: _*)


lazy val scriptModel1 = (project in file("examples/scriptModel1")).
  settings(normalCrossScala: _*).
  dependsOn(core % "test->test;compile->compile").aggregate(core).
  dependsOn(json4s % "test->test;compile->compile").
  //  dependsOn(tagless % "test->test;compile->compile").aggregate(tagless).
  settings(publishArtifact := false).
  settings(pactSettings: _*)

lazy val scriptModel2 = (project in file("examples/scriptModel2")).
  settings(normalCrossScala: _*).
  dependsOn(core % "test->test;compile->compile").aggregate(core).
  dependsOn(json4s % "test->test;compile->compile").
  //  dependsOn(tagless % "test->test;compile->compile").aggregate(tagless).
  settings(publishArtifact := false).
  settings(pactSettings: _*)

lazy val scriptModel3 = (project in file("examples/scriptModel3")).
  settings(normalCrossScala: _*).
  dependsOn(core % "test->test;compile->compile").aggregate(core).
  dependsOn(json4s % "test->test;compile->compile").
  //  dependsOn(tagless % "test->test;compile->compile").aggregate(tagless).
  settings(publishArtifact := false).
  settings(pactSettings: _*)

lazy val scriptBackendShared = (project in file("examples/scriptBackendShared")).
  settings(justScala12).
  dependsOn(core % "test->test;compile->compile").aggregate(core).
  dependsOn(scriptModel1 % "test->test;compile->compile").aggregate(scriptModel1).
  dependsOn(json4s % "test->test;compile->compile").
  //  dependsOn(tagless % "test->test;compile->compile").aggregate(tagless).
  settings(publishArtifact := false).
  settings(pactSettings: _*)

lazy val scriptBackend1 = (project in file("examples/scriptBackend1")).
  settings(justScala12).
  dependsOn(core % "test->test;compile->compile").aggregate(core).
  dependsOn(scriptBackendShared % "test->test;compile->compile").aggregate(scriptBackendShared).
  dependsOn(scriptModel1 % "test->test;compile->compile").aggregate(scriptModel1).
  dependsOn(json4s % "test->test;compile->compile").
  //  dependsOn(tagless % "test->test;compile->compile").aggregate(tagless).
  settings(publishArtifact := false).
  settings(pactSettings: _*)

lazy val scriptBackend2 = (project in file("examples/scriptBackend2")).
  settings(justScala12).
  dependsOn(core % "test->test;compile->compile").aggregate(core).
  dependsOn(scriptBackendShared % "test->test;compile->compile").aggregate(scriptBackendShared).
  dependsOn(scriptModel2 % "test->test;compile->compile").aggregate(scriptModel2).
  dependsOn(json4s % "test->test;compile->compile").
  //  dependsOn(tagless % "test->test;compile->compile").aggregate(tagless).
  settings(publishArtifact := false).
  settings(pactSettings: _*)

lazy val scriptBackend3 = (project in file("examples/scriptBackend3")).
  settings(justScala12).
  dependsOn(core % "test->test;compile->compile").aggregate(core).
  dependsOn(scriptBackendShared % "test->test;compile->compile").aggregate(scriptBackendShared).
  dependsOn(scriptModel3 % "test->test;compile->compile").aggregate(scriptModel3).
  dependsOn(json4s % "test->test;compile->compile").
  //  dependsOn(tagless % "test->test;compile->compile").aggregate(tagless).
  settings(publishArtifact := false).
  settings(pactSettings: _*)

lazy val scriptWebsite = (project in file("examples/scriptWebsite")).
  settings(justScala12).
  dependsOn(core % "test->test;compile->compile").aggregate(core).
  dependsOn(json4s % "test->test;compile->compile").
  dependsOn(mustache % "test->test;compile->compile").
  dependsOn(scriptModel1 % "test->test;compile->compile").aggregate(scriptModel1).
  //  dependsOn(tagless % "test->test;compile->compile").aggregate(tagless).
  settings(publishArtifact := false).
  settings(pactSettings: _*)

lazy val scriptTest = (project in file("examples/scriptTest")).
  settings(normalCrossScala: _*).
  dependsOn(core % "test->test;compile->compile").aggregate(core).
  dependsOn(scriptBackend1 % "test->test;compile->compile").aggregate(scriptBackend1).
  dependsOn(scriptBackend2 % "test->test;compile->compile").aggregate(scriptBackend2).
  dependsOn(scriptBackend3 % "test->test;compile->compile").aggregate(scriptBackend3).
  dependsOn(scriptWebsite % "test->test;compile->compile").aggregate(scriptWebsite).
  settings(publishArtifact := false).
  settings(parallelExecution in Test := false).
  settings(seleniumSettings: _*).
  settings(publishSettings: _*)

//lazy val finatra = (project in file("modules/finatra")).
//  settings(justFinatraCrossScala: _*).
//  dependsOn(core % "test->test;compile->compile").aggregate(core).
//  settings(finatraSettings: _*)
//
//lazy val finatraSample = (project in file("examples/finatraSample")).
//  settings(justFinatraCrossScala: _*).
//  dependsOn(core % "test->test;compile->compile").aggregate(core).
//  dependsOn(finatra % "test->test;compile->compile").aggregate(finatra).
//  dependsOn(sample % "test->test;compile->compile").aggregate(sample).
//  dependsOn(json4s).
//  settings(publishArtifact := false).
//  settings(publishSettings: _*)

//val experimental = (project in file("modules/experimental")).
//  settings(publishSettings).
//  settings(publishArtifact := false).
//  aggregate(
//    cep,
//  )

lazy val scripts = (project in file("modules/scripts")).
  settings(normalCrossScala: _*).
  settings(publishSettings: _*).
  dependsOn(core % "test->test;compile->compile").aggregate(core)


val xingYi = (project in file(".")).
  settings(publishSettings).
  settings(publishArtifact := false, crossScalaVersions := Nil).
  aggregate(
    apachejdbc, //
    core, //
    //    finatra, //
    mustache, //
    //    finatraSample,
    scriptBackend1,
    scriptBackend2,
    scriptBackend3,
    scriptWebsite,
    scientist,
    scripts,
    //    scriptTest,
    sample,
    sampleServer,
    json4s, //
    test
  )
