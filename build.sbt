import sbt._

val monocleVersion = "1.5.0" // 1.5.0-cats based on cats 1.0.x

lazy val renouee = (project in file(".")) settings (
  name := "renouee_v7",
  version := "0.1",
  scalaVersion := "2.12.4",
  libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1",
  libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.2.0",
  libraryDependencies ++= Seq(
    "com.github.julien-truffaut" %%  "monocle-core"  % monocleVersion,
    "com.github.julien-truffaut" %%  "monocle-macro" % monocleVersion,
    "com.github.julien-truffaut" %%  "monocle-law"   % monocleVersion % "test"),
  OsgiKeys.exportPackage := Seq("renouee.*"),
  OsgiKeys.importPackage := Seq("*;resolution:=optional"),
  //OsgiKeys.privatePackage := Seq("*"),  // R
  OsgiKeys.privatePackage := Seq("!scala.*;*"), // OpenMOLE
  OsgiKeys.requireCapability := """osgi.ee;filter:="(&(osgi.ee=JavaSE)(version=1.8))""""
)

enablePlugins (SbtOsgi)

addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)
