lazy val root = (project in file(".")).
  settings(
    name := "data",
    version := "1.0",
    scalaVersion := "2.11.7",
	EclipseKeys.withSource := true,
    resolvers ++= Seq("Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/",
    "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
    ),
	libraryDependencies ++= Seq(
	"joda-time" % "joda-time" % "2.9",
	"org.apache.poi" % "poi" % "3.9",
	"org.jfree" % "jfreechart" % "1.0.14",
	"com.github.wookietreiber" %% "scala-chart" % "latest.integration",
	"com.itextpdf" % "itextpdf" % "5.5.6",
	"com.typesafe.play" %% "play-json" % "2.3.4",
	"org.scalanlp" % "breeze_2.11" % "0.11.2",
	"net.sf.opencsv" % "opencsv" % "2.3",
	"com.squants"  %% "squants"  % "0.5.3",
	"org.apache.commons" % "commons-math3" % "3.6"
	)
	
  )
