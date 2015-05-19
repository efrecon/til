lappend auto_path ../spool
lappend auto_path ../diskutil
package require spool


proc finish { spool } {
    global tmpdir

    puts [::spool::ioctl $spool]
    ::spool::delete $spool
    ::diskutil::clean_directory $tmpdir
    file delete $tmpdir
    exit
}

proc treat { spool fname } {
    global counter

    incr counter
    puts $fname

    if { $counter < 10 } {
	return KEEP
    } else {
	after 5000 finish $spool
	return SENT
    }
}

::spool::loglevel debug

set tmpdir [::diskutil::temporary_directory spool_test]
::spool::rootdir $tmpdir

set s [::spool::new treat]

set fname [::diskutil::temporary_filename spool_test tmp]
set fname [file join [::spool::inbox $s] $fname]
::spool::lock $s
set fd [open $fname "w"]
puts $fd "One test line"
close $fd
::spool::unlock $s


set counter 0
vwait forever
