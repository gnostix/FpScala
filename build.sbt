name := "Kitro"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies ++= {
  val scalaTestV = "3.0.7"
  Seq(
    "org.scalatest" %% "scalatest" % scalaTestV % "test"
  )
}