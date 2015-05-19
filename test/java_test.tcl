lappend auto_path ../java
package require java


::java::loglevel debug
set jvm [::java::find jvm_vernum]
puts "Current JVM is at $jvm, version number is $jvm_vernum"
