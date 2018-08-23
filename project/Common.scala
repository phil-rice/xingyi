import sbt.Keys._
import sbt._
import sbt.plugins.JvmPlugin
object Common extends AutoPlugin {

  override def trigger: PluginTrigger = allRequirements

  override def requires: Plugins = JvmPlugin

  override lazy val projectSettings: Seq[Setting[_]] = Seq(
    version := "0.3",
    organization := "one.xingyi",
    publishMavenStyle := true,
    logLevel := Level.Warn,
    scalaVersion := Libs.scalaVersion,
    scalacOptions ++= Seq("-feature")
  )
}

object Publish extends AutoPlugin {

  override def requires: Plugins = JvmPlugin

  override lazy val projectSettings: Seq[Setting[_]] = Seq(
    pomIncludeRepository := { _ =>
      false
    },
    publishArtifact in Test := false,
    licenses := Seq(
      "BSD-style" -> url("http://www.opensource.org/licenses/bsd-license.php")),
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
    }
  )

}
