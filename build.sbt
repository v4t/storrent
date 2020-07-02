name := "STorrent"

version := "0.0.1"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.5.21",
  "com.typesafe.akka" %% "akka-testkit" % "2.5.21" % Test,
)
libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-slf4j" % "2.6.6",
  "ch.qos.logback" % "logback-classic" % "1.2.3"
)

libraryDependencies +=  "org.scalaj" %% "scalaj-http" % "2.4.1"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"
libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.5" % "test"