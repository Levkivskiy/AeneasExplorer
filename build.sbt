lazy val commonSettings = Seq(
  organization := "AeneasPlatform",
  scalaVersion := "2.12.4",
  version := "0.0.1-alpha"
)
javacOptions ++= Seq("-source", "10", "-target", "10", "--add-modules=java.xml.bind")

resolvers += Resolver.bintrayRepo("parabellum1905y","maven")

val typesafeDependencies = Seq (
  "com.typesafe.akka" %% "akka-http" % "10.1.1",
  "com.typesafe.akka" %% "akka-stream" % "2.5.11",
  "com.typesafe.akka" %% "akka-actor"  % "2.5.11",
  "com.typesafe" % "config" % "1.3.1",
  "com.typesafe.akka" %% "akka-testkit" % "2.5.+" % "test",
  "com.typesafe.akka" %% "akka-http-testkit" % "10.+" % "test",
)

val testDependencies = Seq(
  "org.scalactic" %% "scalactic" % "3.0.3" % "test",
  "org.scalatest" %% "scalatest" % "3.0.3" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.+",
  "net.databinder.dispatch" %% "dispatch-core" % "+" % "test",
  "com.dimafeng" %% "testcontainers-scala" % "0.16.0" % "test",
)

val loggingDependencies = Seq(
  //"org.slf4j" % "slf4j-api" % "1.8.0-beta2",
  "org.slf4j" % "slf4j-api" % "1.7.25",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.+",
  "ch.qos.logback" % "logback-core" % "1.2.3" ,
  "ch.qos.logback" % "logback-classic" % "1.2.3"
)

lazy val circeVersion = "0.9.1"

val scorexDependencies = Seq(
  "org.scorexfoundation" %% "iodb" % "0.3.2",
  "org.scorexfoundation" %% "scrypto" % "2.+" exclude( "org.scorexfoundation", "scorex-util_2.12"),
  //"org.scorexfoundation" %% "scorex-util-fork" % "0.1.1",
  "de.heikoseeberger" %% "akka-http-circe" % "1.19.0"
)

lazy val aeneas = Project(id = "Aeneas", base = file(s"."))
   .dependsOn(scorex)
   .settings(commonSettings: _*)

lazy val scorexUtil = Project(id = "Scorex-util", base = file("scorex-util"))
  .settings(commonSettings: _*)

lazy val scorex = Project(id = "Scorex", base = file("scorex"))
    .dependsOn (scorexUtil)
   .settings(commonSettings: _*)

lazy val bench = Project(id = "Benchmarks", base = file("benchmarks"))
   .dependsOn(aeneas)
   .settings(commonSettings: _*)
   .enablePlugins(JmhPlugin)

libraryDependencies in ThisBuild ++= Seq(
  "com.iheart" %% "ficus" % "1.4.2",
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "io.circe" %% "circe-generic-extras" % circeVersion,
  "org.bitlet" % "weupnp" % "0.1.+",
  "commons-net" % "commons-net" % "3.+"
) ++ testDependencies ++ loggingDependencies ++ typesafeDependencies ++ scorexDependencies

dependencyOverrides in ThisBuild  ++= Seq(
  "ch.qos.logback" % "logback-core" % "1.2.3" ,
  "ch.qos.logback" % "logback-classic" % "1.2.3")

mainClass in assembly := Some("SimpleBlockChain")

(test in Test) := {
  assembly.value
  (test in Test).value
}
logLevel := Level.Warn
test in assembly := {}//TODO
assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case "reference.conf" => MergeStrategy.concat
  case x => MergeStrategy.first
}
