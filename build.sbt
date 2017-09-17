name := "advanced-cats"
organization := "uk.camsw"
scalaVersion := "2.11.8"
version := "1.0.0-SNAPSHOT"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
resolvers ++= Seq("jBCrypt Repository" at "http://repo1.maven.org/maven2/org/")

(testOptions in Test) += Tests.Argument(TestFrameworks.ScalaTest, "-h", "target/scalatest-report")

libraryDependencies += "org.typelevel" % "cats-core_2.11" % "0.7.2"
libraryDependencies += "org.typelevel" % "cats-macros_2.11" % "0.7.2"
libraryDependencies += "org.typelevel" % "cats-kernel_2.11" % "0.7.2"
libraryDependencies += "com.roundeights" % "hasher_2.11" % "1.2.0"

libraryDependencies += "org.pegdown" % "pegdown" % "1.0.2" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.6" % "test"

