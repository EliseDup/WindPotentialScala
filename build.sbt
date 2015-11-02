lazy val root = (project in file(".")).
  settings(
    name := "data",
    version := "1.0",
    scalaVersion := "2.11.7",
	libraryDependencies ++= Seq(
	"joda-time" % "joda-time" % "2.9",
	"org.apache.poi" % "poi" % "3.9",
	"org.jfree" % "jfreechart" % "1.0.14",
	"com.github.wookietreiber" %% "scala-chart" % "latest.integration",
	"com.itextpdf" % "itextpdf" % "5.5.6"
	)
  )
