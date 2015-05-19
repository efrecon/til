lappend auto_path ../../tcllib1.6.1
lappend auto_path ..
package require synchro
package require diskutil

::synchro::loglevel debug

proc getcb { id fname lmtime mtime } {
    global argv0

    if { 0 } {
	if { [file tail $fname] == [file tail $argv0] } {
	    return 0
	} else {
	    return [expr { ($lmtime < $mtime) || $lmtime == 0}]
	}
    } else {
	return [expr int(rand() * 2)]
    }
}

proc overwritecb { id dstfname tmpfname } {
    return [expr int(rand() * 2)]
}


proc added { id fname } {
    puts "ADDED: $fname"
}

set syncdir [::diskutil::temporary_directory synchro_test]

if { 0 } {
    set id1 [::synchro::new $syncdir -newcb added -getcb getcb \
		 -overwritecb overwritecb]
    ::synchro::add $id1 [::diskutil::absolute_path ..]
}

if { 0 } {
    set id2 [::synchro::new $syncdir -newcb added]
    ::synchro::add $id2 ftp://ftp.sunet.se/pub/pictures/people/men
}

if { 1 } {
    set id3 [::synchro::new $syncdir -newcb added]
    ::synchro::add $id3 http://granma.sics.se:8080/
}

vwait forever
