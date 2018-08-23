
lazy val core = (project in file("modules/core")).
  settings(libraryDependencies ++= Dependencies.Core)

lazy val jdbc = (project in file("modules/jdbc")).
  dependsOn(core % "test->test;compile->compile").
  settings(libraryDependencies ++= Dependencies.Jdbc).
  enablePlugins(Publish)

val apachejdbc = (project in file("modules/apachejdbc")).
  dependsOn(core % "test->test;compile->compile").
  settings(libraryDependencies ++= Dependencies.ApacheJdbc)

val cddmustache = (project in file("modules/cddmustache")).
  dependsOn(core % "test->test;compile->compile").
  aggregate(core).
  settings(libraryDependencies ++= Dependencies.CddMustache)

val cddscalatest = (project in file("modules/cddscalatest")).
  dependsOn(core % "test->test;compile->compile").
  dependsOn(cddengine % "test->test;compile->compile").
  settings(libraryDependencies ++= Dependencies.CddScalaTest)


lazy val tagless = (project in file("modules/tagless")).
  dependsOn(core % "test->test;compile->compile").aggregate(core).
  settings(libraryDependencies ++= Dependencies.Tagless).
  enablePlugins(Publish)

lazy val cddscenario = (project in file("modules/cddscenario")).
  dependsOn(core % "test->test;compile->compile").aggregate(core).
  settings(libraryDependencies ++= Dependencies.CddScenario)

lazy val cep = (project in file("modules/cep")).
  dependsOn(core % "test->test;compile->compile").
  aggregate(core).
  settings(libraryDependencies ++= Dependencies.Cep)

val cddexamples = (project in file("modules/cddexamples")).
  dependsOn(core % "test->test;compile->compile").
  dependsOn(cddengine % "test->test;compile->compile").
  dependsOn(cddscalatest % "test->test;compile->compile").
  dependsOn(json4s % "test->test;compile->compile").
  dependsOn(cddmustache % "test->test;compile->compile").
  dependsOn(apachejdbc % "test->test;compile->compile").
  settings(libraryDependencies ++= Dependencies.CddExamples).
  enablePlugins(Publish)

lazy val cddengine = (project in file("modules/cddengine")).
  dependsOn(core % "test->test;compile->compile").aggregate(core).
  dependsOn(cddscenario % "test->test;compile->compile").aggregate(cddscenario).
  enablePlugins(Publish)

lazy val cddscripts = (project in file("modules/cddscripts")).
  dependsOn(core % "test->test;compile->compile").aggregate(core).
  enablePlugins(Publish)

lazy val test = (project in file("modules/test")).
  dependsOn(core % "test->test;compile->compile").
  dependsOn(cep % "test->test;compile->compile").
  dependsOn(apachejdbc % "test->test;compile->compile").
  dependsOn(json4s % "test->test;compile->compile").
  aggregate(core).
  enablePlugins(Publish)

lazy val sampleServer = (project in file("modules/sampleServer")).
  settings(publishArtifact := false).
  dependsOn(core % "test->test;compile->compile").aggregate(core).
  dependsOn(tagless % "test->test;compile->compile").aggregate(tagless).
  dependsOn(sample % "test->test;compile->compile").aggregate(sample).
  dependsOn(jdbc % "test->test;compile->compile").aggregate(sample).
  dependsOn(json4s).
  enablePlugins(Publish)

lazy val finatra = (project in file("modules/finatra")).
  dependsOn(core % "test->test;compile->compile").aggregate(core).
  settings(libraryDependencies ++= Dependencies.Finatra).
  enablePlugins(Publish)

lazy val json4s = (project in file("modules/json4s")).
  dependsOn(core % "test->test;compile->compile").aggregate(core).
  settings(libraryDependencies ++= Dependencies.Json4s)
  enablePlugins(Publish)

lazy val sample = (project in file("modules/sample")).
  dependsOn(core % "test->test;compile->compile").aggregate(core).
  dependsOn(tagless % "test->test;compile->compile").aggregate(tagless).
  settings(publishArtifact := false).
  enablePlugins(Publish)

lazy val finatraSample = (project in file("modules/finatraSample")).
  dependsOn(core % "test->test;compile->compile").aggregate(core).
  dependsOn(finatra % "test->test;compile->compile").aggregate(finatra).
  dependsOn(sample % "test->test;compile->compile").aggregate(sample).
  dependsOn(json4s).
  settings(publishArtifact := false).
  enablePlugins(Publish)

val root = (project in file(".")).
  settings(publishArtifact := false).
  aggregate(
    apachejdbc,
    cddscenario,
    cddengine,
    cddmustache,
    cddscalatest,
    cep,
    core,
    finatra,
    finatraSample,
    sample,
    sampleServer,
    json4s,
    tagless
  ).
  enablePlugins(Publish)
