name := "mgm"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies  ++= Seq(
  "org.scalanlp" %% "breeze" % "0.12",
  "org.scalanlp" %% "breeze-natives" % "0.12",
  "org.scalanlp" %% "breeze-viz" % "0.12",
  "com.typesafe.play" %% "play-json" % "2.5.3",
  "org.specs2" %% "specs2-core" % "3.8.3" % "test",
  "org.specs2" %% "specs2-matcher-extra" % "3.8.3" % "test",
  "com.typesafe.akka" %% "akka-actor" % "2.4.6",
  "ch.qos.logback" %  "logback-classic" % "1.1.7",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.4.0"
)

resolvers ++= Seq(
  // other resolvers here
  // if you want to use snapshot builds (currently 0.12-SNAPSHOT), use this.
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"
)

scalacOptions in Test ++= Seq("-Yrangepos")