name := "Solvers"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "org.scalanlp" %% "breeze" % "0.13.2",
  "org.scalanlp" %% "breeze-natives" % "0.13.2",
  "org.scalatest" %% "scalatest" % "3.0.4" % "test",
  "org.ojalgo" % "ojalgo" % "44.0.0",
  "org.ujmp" % "ujmp-core" % "0.3.0",
  "hu.bme.mit.inf.matrix-toolkits-java" % "mtj" % "1.0.8-SNAPSHOT"
)

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.mavenLocal
)

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:experimental.macros",
  "-unchecked",
  // "-Xfatal-warnings",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture"
)
