import sbt._
import Keys._

object GeoScalaBuild extends Build {
  def module(name: String) =
    Project(name, file(name), settings = Seq(
      scalaVersion  := "2.11.7"
    ))

  lazy val root = Project("root", file("."))
    .aggregate(geometry, geodesy)

  lazy val geometry = module("geometry")

  lazy val geodesy = module("geodesy").settings(
    libraryDependencies += "org.orbisgis" % "cts" % "1.3.3"
  ).dependsOn(geometry)
}

