name := "scala-playground"

version := "1.0"

scalaVersion := "2.11.4"

resolvers ++= Seq(
  "Sonatype OSS Releases"  at "http://oss.sonatype.org/content/repositories/releases/",
  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
)


libraryDependencies ++= Seq(
//  "com.chuusai" % "shapeless" % "2.0.0-M1" cross CrossVersion.full
  "com.chuusai" % "shapeless_2.11" % "2.0.0" withSources()
//  "com.chuusai" % "shapeless_2.10.2" % "2.0.0-M1" // alternatively ...
)

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.0" withSources()

libraryDependencies += "org.scala-lang.modules" %% "scala-async" % "0.9.2"

// libraryDependencies += "com.typesafe" %% "scalalogging-slf4j" % "1.0.1"
libraryDependencies += "com.typesafe.scala-logging" % "scala-logging_2.11" % "3.1.0"

//libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.5"

//libraryDependencies += "org.apache.commons" % "commons-math3" % "3.0"
libraryDependencies += "org.apache.commons" % "commons-math3" % "3.3"

//libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"
libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.2"


//libraryDependencies += "org.scala-lang" % "scala-swing" % "2.10.1"

