name := "scalatoprolog"

version := "0.1.0"

scalaVersion := "2.13.3"

libraryDependencies ++= Seq(

  "it.unibo.alice.tuprolog" % "tuprolog" % "3.3.0",
  "org.typelevel" %% "cats-effect" % "2.2.0"

)