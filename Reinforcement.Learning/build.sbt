name := "Reinforcement Learning"

version := "1.0"

scalaVersion := "2.12.3"



libraryDependencies  ++= Seq(
 "org.scala-lang.modules" %% "scala-swing" % "2.0.0-M2",
// Last stable release
  "org.scalanlp" %% "breeze" % "0.13.2",
  //"org.scalanlp" %% "breeze" % "2.10",

  // Native libraries are not included by default. add this if you want them (as of 0.7)
  // Native libraries greatly improve performance, but increase jar sizes.
  // It also packages various blas implementations, which have licenses that may or may not
  // be compatible with the Apache License. No GPL code, as best I know.
  "org.scalanlp" %% "breeze-natives" % "0.13.2",
  //"org.scalanlp" %% "breeze-natives" % "2.10",

  // The visualization library is distributed separately as well.
  // It depends on LGPL code
  "org.scalanlp" %% "breeze-viz" % "0.13.2",
  "net.ericaro" % "surfaceplotter" % "2.0.1"
 //"com.googlecode.surfaceplotter" % "surfaceplotter" % "2.0.0.beta2"
  //"org.scalanlp" %% "breeze-math" % "0.13.2"
  //"org.scalanlp" %% "breeze-learn" % "0.13.2"
  //"org.scalanlp" %% "breeze-core" % "0.13.2"
)


resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
//resolvers += "maven Releases" at "https://repo1.maven.org/maven2/"
