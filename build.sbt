scalaVersion in ThisBuild := "2.12.8"
scalacOptions in ThisBuild ++= Seq(
  "-language:_",
  "-Ypartial-unification",
  "-Xfatal-warnings"
)

libraryDependencies ++= Seq(
  "com.github.mpilquist" %% "simulacrum" % "0.13.0",
  "org.scalaz" %% "scalaz-core" % "7.2.26",
  "com.propensive" %% "contextual" % "1.1.0",
  "eu.timepit" %% "refined" % "0.9.2",
  "eu.timepit" %% "refined-scalaz" % "0.9.2",
  "io.argonaut" %% "argonaut" % "6.2.3",
  "org.scalactic" %% "scalactic" % "3.0.5",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7")
addCompilerPlugin(
  "org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full
)
