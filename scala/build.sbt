val scala3Version = "3.2.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "effects",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    scalacOptions ++= Seq("-Ykind-projector:underscores"),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.8.0",
      "org.typelevel" %% "cats-free" % "2.8.0",
      "org.typelevel" %% "cats-effect" % "3.3.12",
      "org.scalatest" %% "scalatest" % "3.2.14" % Test
    )
  )
