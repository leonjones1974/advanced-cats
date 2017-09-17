
logLevel := sbt.Level.Info
addSbtPlugin("com.waioeka.sbt" % "cucumber-plugin" % "0.0.8")
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.3")
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.8.2")
resolvers += "spray repo" at "http://repo.spray.io"
addSbtPlugin("io.spray" % "sbt-twirl" % "0.7.0")
