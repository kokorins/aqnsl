name := "mgm"

version := "1.0"

scalaVersion := "2.12.1"

val breezeVersion = "0.13.1"

val scalaGraphVersion = "1.11.5"

libraryDependencies  ++= Seq(
  "org.scalanlp" %% "breeze" % breezeVersion,
  "org.scalanlp" %% "breeze-natives" % breezeVersion,
  "org.scalanlp" %% "breeze-viz" % breezeVersion,
  "com.typesafe.play" %% "play-json" % "2.6.0-M1",
  "ch.qos.logback" %  "logback-classic" % "1.1.7",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
  "com.google.guava" % "guava" % "20.0",
  "org.scala-graph" %% "graph-core" % scalaGraphVersion,
  "org.scala-graph" %% "graph-dot" % scalaGraphVersion
)

resolvers ++= Seq(
  // other resolvers here
  // if you want to use snapshot builds (currently 0.12-SNAPSHOT), use this.
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"
)

scalacOptions in Test ++= Seq("-Yrangepos")