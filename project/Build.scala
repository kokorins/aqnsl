import sbt._

object mgmBuild extends Build {
  lazy val root = Project(id = "mgm", base = file(".")).aggregate(galileo).dependsOn(galileo)
  lazy val galileo = Project(id = "galileo", base = file("galileo"))
}
