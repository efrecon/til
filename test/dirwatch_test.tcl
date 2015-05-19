lappend auto_path ../dirwatch
package require dirwatch
package require fileutil

proc newfile { w fname } {
    puts "New file created: $fname"
}

proc delfile { w fname } {
    puts "File removed: $fname"
}

::dirwatch::loglevel debug
set dirname [::fileutil::tempdir]

set dw [::dirwatch::new $dirname newfile delfile]

set fname [::fileutil::tempfile]
after 1000 file delete $fname

after 2000 ::dirwatch::delete $dw
after 2000 exit

vwait forever

