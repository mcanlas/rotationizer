lazy val `rotationizer` =
  project
    .in(file("."))
    .aggregate(core, solver, demo)

lazy val core =
  module("core")
    .withEffectMonad
    .withTesting

lazy val solver =
  module("solver").withTesting

lazy val demo =
  module("demo")
    .dependsOn(core, solver)
    .withTesting
