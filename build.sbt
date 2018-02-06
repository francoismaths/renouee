import sbt._

lazy val renouee = (project in file(".")) settings (
  name := "renouee_v7",
  version := "0.1",
  scalaVersion := "2.12.4",
  libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1",
  libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.2.0",
  OsgiKeys.exportPackage := Seq("renouee.*"),
  OsgiKeys.importPackage := Seq("*;resolution:=optional"),
  //OsgiKeys.privatePackage := Seq("*"),  // R
  OsgiKeys.privatePackage := Seq("!scala.*;*"), // OpenMOLE
  OsgiKeys.requireCapability := """osgi.ee;filter:="(&(osgi.ee=JavaSE)(version=1.8))""""
) enablePlugins (SbtOsgi)
