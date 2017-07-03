enablePlugins(ScalaJSPlugin)

name := "DualNBack"

version := "1.0"

scalaVersion := "2.12.2"

resolvers += Resolver.sonatypeRepo("releases")
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

libraryDependencies ++= Seq(
  "org.typelevel" %%% "cats" % "0.9.0",
  "com.chuusai" %%% "shapeless" % "2.3.3",

  "org.scala-js" %%% "scalajs-dom" % "0.9.2",
  "com.lihaoyi" %%% "scalatags" % "0.6.7"
)
