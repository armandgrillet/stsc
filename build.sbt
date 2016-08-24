organization := "gr.armand"
name := "stsc"

version := "1.0"

scalaVersion := "2.11.8"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/releases"

mainClass in Compile := Some("stsc.Job")

libraryDependencies  ++= Seq(
    // Breeze.
    "org.scalanlp" %% "breeze" % "0.12" % "provided",
    "org.scalanlp" %% "breeze-natives" % "0.12" % "provided",
    // The unit test library.
    "org.scalactic" %% "scalactic" % "2.2.6",
    "org.scalatest" %% "scalatest" % "2.2.6" % "test",
    // Benchmarks
    "com.storm-enroute" %% "scalameter" % "0.7",
    // Spark
    "org.apache.spark" %% "spark-core" % "2.0.0" % "provided",
    // Hadoop HDFS to merge the results.
    "org.apache.hadoop" % "hadoop-client" % "2.6.4",
    "org.apache.hadoop" % "hadoop-hdfs" % "2.6.4"
    // Logs
    //"org.slf4j" % "slf4j-simple" % "1.6.4"
)

// Assembly
mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
   {
    case PathList("META-INF", xs @ _*) => MergeStrategy.discard
    case x => MergeStrategy.first
   }
}
test in assembly := {}

// Tests
testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")
logBuffered := false
