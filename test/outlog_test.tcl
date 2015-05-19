lappend auto_path ../outlog
package require outlog
package require fileutil

set tmp_fname [::fileutil::tempfile outlog_test]
::outlog::loglevel debug
set ol [::outlog::open $tmp_fname]
::outlog::puts $ol "test line"
::outlog::close $ol

if { [file exists $tmp_fname] && [file size $tmp_fname] > 0 } {
    puts "Test passed"
    file delete $tmp_fname
} else {
    puts "Test failed"
}
