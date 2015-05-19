source [file join [file dirname $argv0] .. bin argutil.tcl]

set vernum [argutil_accesslib tcllib]
puts $vernum
puts $auto_path
