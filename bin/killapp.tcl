#!/bin/sh
# the next line restarts using tclsh \
exec tclsh "$0" "$@"

##################
## Program Name    --  killapp.tcl
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##   This program implements kill (or simply restart) an application running
##   under the control of the starter process.
##
##################
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.


# Array Name       --  KA
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Global state of this program
#
# Contents:
#    services	- List of started services
#    finish	- End indicator
#    infile	- File to initiate with on start up.
array set KA {
    host "localhost"
    port 3448
    verbose "warn"
    restart 0
    reload  0
    sync    0
    finish  0
    killstarter 0
}

source [file join [file dirname $argv0] argutil.tcl]
argutil::accesslib tcllib

# Now parse the options and put the result into the KA global state array
package require cmdline

set options "host.arg port.integer verbose.arg restart reload sync kill"
set opt_p 0
while { [set err [cmdline::typedGetopt argv $options opt arg]] } {
    if { $err == 1 } {
	if { $opt == "host" } {
	    set KA(host) $arg
	    incr opt_p 2
	} elseif { $opt == "port" } {
	    set KA(port) $arg
	    incr opt_p 2
	} elseif { $opt == "verbose" } {
	    set KA(verbose) $arg
	    incr opt_p 2
	} elseif { $opt == "restart" } {
	    set KA(restart) 1
	    incr opt_p 1
	} elseif { $opt == "reload" } {
	    set KA(reload) 1
	    incr opt_p 1
	} elseif { $opt == "sync" } {
	    set KA(sync) 1
	    incr opt_p 1
	} elseif { $opt == "kill" } {
	    set KA(killstarter) 1
	    incr opt_p 1
	}
    } elseif { $err < 0 } {
	puts "ERROR: $opt"
	exit
    }
}

# Initialise local logging facility
package require logger
set KA(log) [::logger::init killapp]
$KA(log)::setlevel $KA(verbose)

if { [catch "socket $KA(host) $KA(port)" s] == 0 } {
    set wait 0
    if { $KA(reload) } {
	puts $s "RELOAD"
	set wait $KA(sync)
    } elseif { $KA(restart) } {
	if { [llength $argv] > 0 } {
	    puts $s "RESTART $argv"
	} else {
	    puts $s "RESTART_ALL"
	}
    } else {
	if { [llength $argv] > 0 } {
	    puts $s "REMOVE $argv"
	} else {
	    puts $s "REMOVE_ALL"
	}
	set wait $KA(sync)
    }
    flush $s

    if { $KA(sync) } {
	set line [gets $s]
	if { [lindex $line 0] == "KILLED" } {
	    set nb_pids [expr [llength $line] - 1]
	    $KA(log)::info \
		"Killed $nb_pids processes, pids: [lrange $line 1 end]"
	}
    }

    if { $KA(killstarter) } {
	puts $s "EXIT"
	flush $s
    }

    close $s
} else {
    $KA(log)::warn \
	"Could not open socket connection to starter at $KA(host):$KA(port)!"
}
