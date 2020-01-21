name := "SpinalHDL-HardFloat"

version := "1.0"

scalaVersion := "2.11.12"

EclipseKeys.withSource := true

libraryDependencies ++= Seq(
  "com.github.spinalhdl" % "spinalhdl-core_2.11" % "1.3.8",
  "com.github.spinalhdl" % "spinalhdl-lib_2.11" % "1.3.8",
  "org.scalactic" %% "scalactic" % "3.0.8",
  "org.scalatest" %% "scalatest" % "3.0.8" % "test",  
// math stuff
  "org.scalanlp" %% "breeze" % "1.0",
  "org.scalanlp" %% "breeze-natives" % "1.0",
  "org.typelevel" %% "spire" % "0.14.1"
)

fork := true
