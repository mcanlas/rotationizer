import sbt.Keys.*
import sbt.*

/**
  * Automatically enriches projects with the following settings (despite the word "override").
  */
object ProjectPlugin extends AutoPlugin {

  /**
    * Defines what members will be imported to the `build.sbt` scope.
    */
  val autoImport = ThingsToAutoImport

  /**
    * Thus plug-in will automatically be enabled; it has no requirements.
    */
  override def trigger: PluginTrigger = AllRequirements

  object ThingsToAutoImport {
    private def jarName(s: String) =
      "rotationizer-" + s

    def module(s: String): Project =
      Project(s, file(jarName(s)))
        .settings(name := jarName(s))

    implicit class ProjectOps(p: Project) {
      def withEffectMonad: Project =
        p
          .settings(libraryDependencies += "org.typelevel" %% "cats-effect" % "3.6.0")

      def withTesting: Project = {
        val weaverVersion =
          "0.8.4"

        p.settings(
          libraryDependencies ++= Seq(
            "com.disneystreaming" %% "weaver-cats"       % weaverVersion % Test,
            "com.disneystreaming" %% "weaver-scalacheck" % weaverVersion % Test
          )
        )
      }
    }
  }
}
