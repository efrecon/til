lappend auto_path ../../tcllib1.6.1
lappend auto_path ..
package require diskutil
package require lockfile

::lockfile::loglevel debug

proc lockresult { id status fname } {
    switch $status {
	ERROR {
	    puts "ERROR: $fname"
	}
	LOCKED {
	    puts "LOCK created: $fname, releasing at once"
	    ::lockfile::release $id
	}
    }
    exit
}

set lockf [::lockfile::name]

set id [::lockfile::acquire $lockf]
after 12000 ::lockfile::release $id

set id2 [::lockfile::acquire $lockf -callbacks lockresult]

vwait forever
