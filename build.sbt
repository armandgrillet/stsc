organization := "gr.armand"
name := "stsc"

version := "1.0"

scalaVersion := "2.11.8"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/releases"

libraryDependencies  ++= Seq(
    // other dependencies here
    "org.scalanlp" %% "breeze" % "0.12",
    // native libraries are not included by default. add this if you want them (as of 0.7)
    // native libraries greatly improve performance, but increase jar sizes.
    // It also packages various blas implementations, which have licenses that may or may not
    // be compatible with the Apache License. No GPL code, as best I know.
    "org.scalanlp" %% "breeze-natives" % "0.12",
    // the visualization library is distributed separately as well.
    // It depends on LGPL code.
    // The unit test library.
    "org.scalactic" %% "scalactic" % "2.2.6",
    "org.scalatest" %% "scalatest" % "2.2.6" % "test",
    // Benchmarks
    "com.storm-enroute" %% "scalameter" % "0.7",
    // Spark
    "org.apache.spark" %% "spark-core" % "2.0.0"
    // Logs
    //"org.slf4j" % "slf4j-simple" % "1.6.4"
)

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")
logBuffered := false
