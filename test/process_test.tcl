lappend auto_path ../process
lappend auto_path ../diskutil
package require process

::process::loglevel debug

set running [::process::list]
if { [lsearch $running [pid]] >= 0 } {
    puts "Found our process identifiers among the running ones."
}

foreach pinfo [::process::full_list] {
    if { [lindex $pinfo 0] == [pid] } {
	puts "Info about our process: $pinfo"
    }
}

foreach tsh [::process::find tclsh] {
    if { $tsh == [pid] } {
	puts "We have been started from the tcl shell"
    }
}

puts "Killing ourselves!"
::process::kill [pid]

puts "This message should not appear!"
