name := "storrent"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.5.19",
  "com.typesafe.akka" %% "akka-testkit" % "2.5.19" % Test
)

libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.5" % "test"