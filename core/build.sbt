
enablePlugins(Antlr4Plugin)

lazy val antrl4_version = "4.8-1"

//antlr4Version in Antlr4 := "4.8-1" // default: 4.8-1
Antlr4 / antlr4Version := antrl4_version
Antlr4 / antlr4GenVisitor := true
Antlr4 / antlr4GenListener  := false
// https://mvnrepository.com/artifact/org.antlr/antlr4-runtime
libraryDependencies += "org.antlr" % "antlr4-runtime" % antrl4_version
Antlr4 / antlr4PackageName := Some("com.skyfly")
//javaSource in Antlr4 := (sourceManaged in Compile).value

Antlr4 / javaSource := (Compile / sourceManaged).value
//antlr4GenVisitor in Antlr4 := false // default: false