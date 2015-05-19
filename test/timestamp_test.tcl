source ../bin/argutil.tcl
argutil_accesslib til
argutil_loadmodules timestamp debug

set now1 [::timestamp::get sec1 msec1]
puts "Now: $now1"
set now2 [::timestamp::scan $now1 sec2 msec2]
puts "Now is also: $now2"
