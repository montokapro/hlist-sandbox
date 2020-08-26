val scalatestVersion = "3.0.5"
val shapelessVersion = "2.3.3"
val kittensVersion = "2.1.0"
val catsCollectionVersion = "0.9.0"

lazy val root = (project in file("."))
  .settings(
    organization := "io.github.montokapro",
    name := "hlist-sandbox",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "2.12.10",
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % shapelessVersion,
      "org.typelevel" %% "cats-collections-core" % catsCollectionVersion,
      "org.typelevel" %% "kittens" % "2.1.0",
      "org.scalatest" %% "scalatest" % scalatestVersion % Test
    )
  )

scalacOptions += "-Ypartial-unification"
