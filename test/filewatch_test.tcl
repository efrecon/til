lappend auto_path ../dirwatch
package require filewatch
package require fileutil

proc dumpfile { fname } {
    global count

    incr count
    set fd [open $fname "a+"]
    puts $fd $count
    close $fd
    puts "Dumped $count to $fname"
    if { $count < 100 } {
	after 200 dumpfile \$fname
    }
}


proc wcb { type w fname } {
    switch $type {
	NEW {
	    puts "New file created: $fname"
	}
	NOCHANGE {
	    puts "File has stopped changing: $fname"
	    ::filewatch::delete $w
	    file delete $fname
	    after idle exit
	}
    }   
}


::filewatch::loglevel debug
set fname [::fileutil::tempfile]
#start outputing stupid data to the file
set count 0
after 200 dumpfile \$fname

set w [::filewatch::new $fname "wcb NEW" "wcb NOCHANGE"]
after 2000 ::filewatch::pause $w
after 5000 ::filewatch::resume $w

vwait forever
