course := "progfun1"
assignment := "recfun"

scalaVersion := "3.2.2"

scalacOptions ++= Seq("-language:implicitConversions", "-deprecation")

libraryDependencies += "org.scalameta" %% "munit" % "0.7.26" % Test
