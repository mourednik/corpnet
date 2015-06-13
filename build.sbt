name := "corpnet"

version := "34"

scalaVersion := "2.11.6"

resolvers ++= Seq(
  "Maven Central Server" at "http://repo1.maven.org/maven2",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
)

libraryDependencies  ++= Seq(
  "org.scala-lang" % "scala-compiler" % "2.11.6",
  "org.scala-lang.modules" %% "scala-swing" % "2.0.0-M2",
  "org.scala-lang.modules" %% "scala-pickling" % "0.10.0",
  "com.typesafe.akka" %% "akka-actor" % "2.3.10",
  "org.piccolo2d" % "piccolo2d-core" % "3.0",
  "org.piccolo2d" % "piccolo2d-extras" % "3.0",
  "org.abego.treelayout" % "org.abego.treelayout.core" % "1.0.2",
  "org.scalanlp" %% "breeze" % "0.11.2",
  "org.scalanlp" %% "breeze-natives" % "0.11.2",
  "org.scalanlp" %% "breeze-viz" % "0.11.2",
  "com.bizo" % "mighty-csv_2.11" % "0.2",
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
)

packSettings

packMain := Map("corpnet" -> "corpnet.Main")

