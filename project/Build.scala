import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "mintpresso-apps"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    // Add your project dependencies here,
    jdbc,
    anorm,
    "mysql" % "mysql-connector-java" % "5.1.25"
  )


  val main = play.Project(appName, appVersion, appDependencies).settings(
    lessEntryPoints := Nil, coffeescriptEntryPoints := Nil, javascriptEntryPoints := Nil
  )

}
