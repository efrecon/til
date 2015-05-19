lappend auto_path ../logwatch
package require logwatch
package require fileutil

set tmp_fname [::fileutil::tempfile logwatch_test]

proc dump { id line } {
    puts $line
}

proc finish { lw fname } {
    puts "Finishing up: [::logwatch::info $lw]"
    ::logwatch::delete $lw
    file delete $fname
    exit
}


proc output { fname period counter } {
    set fd [open $fname "a+"]
    puts $fd "Counter $counter"
    close $fd
    incr counter
    after $period output $fname $period $counter
}


after 1000 output $tmp_fname 100 0

::logwatch::loglevel debug
set lw [::logwatch::new $tmp_fname dump "Counter"]

after 20000 finish $lw $tmp_fname

vwait forever


