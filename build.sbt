name := "advent-of-code"

version := "0.1"

scalaVersion := "3.5.1"

libraryDependencies ++= Seq(
  "org.apache.commons" % "commons-lang3" % "3.17.0",
  "com.softwaremill.sttp.client3" %% "core" % "3.10.1",
  "com.softwaremill.sttp.client3" %% "circe" % "3.10.1",
  "io.circe" %% "circe-generic" % "0.14.10",
  "org.typelevel" %% "cats-core" % "2.12.0"
)
