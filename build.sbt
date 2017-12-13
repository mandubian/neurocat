scalaOrganization in ThisBuild := "org.typelevel"

scalaVersion in ThisBuild := "2.12.4-bin-typelevel-4"

scalacOptions in ThisBuild ++= Seq(
  "-Yliteral-types"
)

libraryDependencies ++= Seq(
  "org.typelevel"   %% "cats-core"            % "1.0.0-RC1"
, "com.chuusai"     %% "shapeless"            % "2.3.2"
, "org.typelevel"   %% "spire"                % "0.14.1"
, "org.nd4j"        %  "nd4j-native-platform" % "0.7.2"
, "eu.timepit"      %% "singleton-ops"        % "0.2.2"
, "io.monix"        %% "minitest"             % "2.0.0"       % "test"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

testFrameworks += new TestFramework("minitest.runner.Framework")

fork := true
