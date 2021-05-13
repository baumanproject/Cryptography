name := "Crypto_1_sbt"

version := "0.1"

scalaVersion in ThisBuild := "2.13.5"

//idePackagePrefix := Some("org.mai.crypto")

libraryDependencies += "org.typelevel" %% "spire" % "0.17.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.3" % Test