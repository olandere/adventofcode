lazy val root = (project in file("."))
  .settings(
    name := "adventofcode",
    version := "0.1",
    scalaVersion := "3.1.0",
  //  resourceDirectory in Compile := baseDirectory.value / "resources",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.3.3",
      "org.scalactic" %% "scalactic" % "3.2.10",
      "org.scalatest" %% "scalatest" % "3.2.10" % "test",
      "org.scalatest" %% "scalatest-funspec" % "3.2.10" % "test",
      "org.scalatest" %% "scalatest-funsuite" % "3.2.10" % "test"
    ),
//    scalacOptions ++= Seq(
//      "-Ytasty-reader"
//    )
  )
