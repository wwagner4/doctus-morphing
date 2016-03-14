import sbt._
import Keys._
import sbt.Package.ManifestAttributes
import com.typesafe.sbteclipse.plugin.EclipsePlugin._

import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

object MorphingBuild extends Build {

  // Constant values
  object D {

    val version = "0.1.0-SNAPSHOT"

    val scalaVersion = "2.11.8"
    val doctusVersion = "1.0.5-SNAPSHOT"
    val utestVersion = "0.4.1"
    val scalaJsDomJqueryVersion = "0.9.0"

  }

  // Settings
  object S {

    lazy val commonSettings =
        Seq(
          version := D.version,
          scalaVersion := D.scalaVersion,
          organization := "net.entelijan",
          // TODO replace with github resolvers += "entelijan" at "http://entelijan.net/artifactory/repo",
          EclipseKeys.withSource := true)

    lazy val coreSettings =
      commonSettings ++
        Seq(
          libraryDependencies += "net.entelijan" %%% "doctus-core" % D.doctusVersion,
          libraryDependencies += "com.lihaoyi" %%% "utest" % D.utestVersion % "test"
//          testFrameworks += new TestFramework("utest.runner.Framework")
          )

    lazy val swingSettings =
      commonSettings ++
        Seq(
          fork := true,
          libraryDependencies += "net.entelijan" %% "doctus-swing" % D.doctusVersion,
          libraryDependencies += "com.lihaoyi" %%% "utest" % D.utestVersion % "test",
          testFrameworks += new TestFramework("utest.runner.Framework"))

    lazy val scalajsSettings =
      commonSettings ++
        Seq(
          jsDependencies += RuntimeDOM,
          libraryDependencies += "org.scala-js" %%% "scalajs-dom" % D.scalaJsDomJqueryVersion,
          libraryDependencies += "be.doeraene" %%% "scalajs-jquery" % D.scalaJsDomJqueryVersion,
          libraryDependencies += "com.lihaoyi" %%% "utest" % D.utestVersion % "test",
          libraryDependencies += "net.entelijan" %%% "doctus-scalajs" % D.doctusVersion
//          testFrameworks += new TestFramework("utest.runner.Framework")
          )

  }

  // Project definitions
  lazy val root = Project(
    id = "morphing-root",
    base = file("."), 
    settings = S.commonSettings)
    .aggregate(core, swing, scalajs)

  lazy val core = Project(
    id = "morphing-core",
    base = file("morphing-core"),
    settings = S.coreSettings)
    .enablePlugins(ScalaJSPlugin)

  lazy val swing = Project(
    id = "morphing-swing",
    base = file("morphing-swing"),
    settings = S.swingSettings)
	  .dependsOn(core)

  lazy val scalajs = Project(
    id = "morphing-scalajs",
    base = file("morphing-scalajs"),
    settings = S.scalajsSettings)
	  .dependsOn(core)
    .enablePlugins(ScalaJSPlugin)

}

