name := "arbor"

version := "0.1"

scalaVersion := "2.12.8"

resolvers += Resolver.sonatypeRepo("releases")


addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)

libraryDependencies ++= Seq(
  "org.rogach" %% "scallop" % "3.1.2",
  "com.chuusai" %% "shapeless" % "2.3.3",
  "org.scalaj" %% "scalaj-http" % "2.4.0",
  "io.argonaut" %% "argonaut" % "6.2.1",
  "com.typesafe" % "config" % "1.3.2",
  "com.chuusai" %% "shapeless" % "2.3.3",
  "com.github.alexarchambault" %% "argonaut-shapeless_6.2" % "1.2.0-M4",
  "org.typelevel" %% "cats-core" % "1.5.0",

  "eu.timepit" %% "refined" % "0.9.3",
  "eu.timepit" %% "refined-cats" % "0.9.3", // optional

  "com.github.mpilquist" %% "simulacrum" % "0.14.0",

  "org.typelevel" %% "cats-core" % "1.6.0",
)
