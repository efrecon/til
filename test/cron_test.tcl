lappend auto_path ../cron
package require cron

proc sometimes { when } {
    global counter
    global c

    puts "Callback at $when"
    incr counter
    if { $counter > 3 } {
	puts [::cron::info $c]
	::cron::delete $c

	exit
    }
}

set counter 0
::cron::loglevel debug
set c [::cron::add * * * * * sometimes]


vwait forever
