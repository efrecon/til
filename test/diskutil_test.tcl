lappend auto_path ../diskutil
package require diskutil

::diskutil::loglevel debug

::diskutil::signature ../../til -ignoretop on \
    -ignore ".*~$ .*bak$ .*tmp$" -compute "size name"

puts [::diskutil::double_backslash "\\a\\very stupid\\windows\\path"]

puts [::diskutil::absolute_path "../."]

::diskutil::append_path p [list /usr/tmp /usr]
puts $p

set tmpdir [::diskutil::platform_tmp]
puts "Temporary directory is at: $tmpdir"

set fname [::diskutil::temporary_file "diskutil_test" "tmp"]
set fd [open $fname "w"]
puts $fd "A single line"
close $fd
puts "Created temporary file $fname"

set tname [file tail $fname]
puts "Found $tname as [::diskutil::expand_filename $tmpdir $tname]"

set cleaned [::diskutil::clean_directory $tmpdir [list "diskutil_test*"]]
if { $cleaned > 0 } {
    puts "Managed to remove $cleaned temporary file(s) through a convoluted method!"
}

puts [::diskutil::fname_resolv "An inexisting filepath that show which %os% and %platform% that we run on, from within which %progname% and %progdir% and on which %hostname%"]


proc create_file { fname } {
    set fd [open $fname "w"]
    puts $fd [clock format [clock seconds]]
    close $fd
}


puts "Testing rotation with number in input file"

create_file __diskutil_test.0.log
puts "First rotation"
::diskutil::rotate __diskutil_test.0.log 2

create_file __diskutil_test.0.log
puts "Second rotation"
::diskutil::rotate __diskutil_test.0.log 2

create_file __diskutil_test.0.log
puts "Third rotation"
::diskutil::rotate __diskutil_test.0.log 2

create_file __diskutil_test.0.log
puts [glob __diskutil_test.*.log]

file delete [glob __diskutil_test.*.log]


puts "Testing rotation without number in input file"

create_file __diskutil_test.log
puts "First rotation"
::diskutil::rotate __diskutil_test.log 2

create_file __diskutil_test.log
puts "Second rotation"
::diskutil::rotate __diskutil_test.log 2

create_file __diskutil_test.log
puts "Third rotation"
::diskutil::rotate __diskutil_test.log 2

create_file __diskutil_test.log
puts [glob __diskutil_test.log*]

file delete [glob __diskutil_test.log*]
