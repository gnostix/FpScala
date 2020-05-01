name := "Kitro"

version := "0.1"

scalaVersion := "2.13.1"

libraryDependencies ++= {
  val scalaTestV = "3.1.1"
  Seq(
    "org.scalatest" %% "scalatest" % scalaTestV % "test",
    "org.scalactic" %% "scalactic" % scalaTestV
  )
}