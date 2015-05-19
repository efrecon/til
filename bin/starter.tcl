#!/bin/sh
# the next line restarts using tclsh \
exec tclsh "$0" "$@"

##################
## Program Name    --  starter.tcl
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##   This program implements some sort of Tcl service: it reads a
##   configuration file at startup and ensures that all processes pointed
##   at in the file will keep running.  While this program is Tcl/Tk
##   oriented, it can be used as a watchguard for almost any type of
##   processes, including Java processes or batch files.
##
##################
# Copyright (c) 2004-2007 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.


# Array Name       --  ST
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Global state of this program
#
# Contents:
#    services	- List of started services
#    finish	- End indicator
array set ST {
    servid         -1
    pwid           ""
    dns            "ns.sics.se"
    finish         0
}

source [file join [file dirname $argv0] argutil.tcl]
argutil::accesslib tcllib

# Now parse the options and put the result into the ST global state array
package require cmdline

set options {
    { verbose.arg "warn" "Verbosity Level" }
    { port.integer "3448" "Port to listen to incoming client connections" }
    { pause.integer "1" "Pause between service starts on startup (in secs)" }
    { watch.integer "10" "Period for service watching (in secs)" }
    { infiles.arg "services.stp services.%hostname%.stp" "Files to read initial state from" }
    { allow.arg "*" "List of hostname/ip allow patterns for incoming clients" }
    { deny.arg "" "List of hostname/ip deny patterns for incoming clients" }
    { logfmt.arg "%progname%.%hostname%.log" "Formatting rule for name of log files, when applicable" }
    { log.arg "log" "subdirectory for logging of services output, empty to disable (sub of 'home')" }
    { run.arg "run" "subdirectory for running services information, empty to disable (sub of 'home')" }
    { maxsize.integer "65536" "Max memory size allowed for processes (in kb) (<= 0 to disable)" }
    { home.arg "%progdir%" "Home directory for log/state files" }
}

set inited [argutil::initargs ST $options]
if { [catch {cmdline::typedGetoptions argv $options} optlist] != 0 } {
    puts [cmdline::typedUsage $options "accepts the following options:"]
    exit
}
array set ST $optlist
foreach key $inited {
    argutil::makelist ST($key)
}

# Include modules that we depend on.  This is complicated to be able
# to address separately modules in the verbose specification.
argutil::accesslib til
argutil::accesslib tcludp
argutil::loadmodules \
    [list process cmdserver diskutil procwatch] $ST(verbose)
package require struct::list

# Initialise local logging facility
package require logger
set ST(_log) [::logger::init starter]
$ST(_log)::setlevel $ST(verbose)

argutil::fix_outlog


# Command Name     --  log_out
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Command called back when log lines have been detected.
#
# Arguments:
#    wid	- Identifier of watch
#    line	- Line of log
proc log_out { sock wid line } {
    global ST

    set sid [::procwatch::get $wid service]
    ::procwatch::isa $sid service srid
    ::cmdserver::broadcast $ST(servid) "LOG $srid $line" $sock
}



# Command Name     --  incoming
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Procedure called back on incoming commands from clients.
#
# Arguments:
#    arg1	-
#    arg2	-
proc incoming { sock cmd remaining } {
    global ST
    
    switch $cmd {
	LIST {
	    set svcs [list]
	    foreach sid [::procwatch::get $ST(pwid) services] {
		::procwatch::isa $sid service srid
		lappend svcs $srid [lindex [::procwatch::get $sid pid] 0]
	    }
	    ::cmdserver::broadcast $ST(servid) "LIST $svcs" $sock
	}
	INFO {
	    foreach id $remaining {
		set sid [::procwatch::find $id]
		if { $sid ne "" } {
		    set response "INFO "
		    ::procwatch::isa $sid service srid
		    append response "$srid "
		    append response "\"[::procwatch::get $sid pid]\" "
		    append response "\"[::procwatch::get $sid logfile]\" "
		    append response "\"[::procwatch::get $sid exe]\" "
		    append response "\"[::procwatch::get $sid -main]\" "
		    append response "\"[::procwatch::get $sid -args]\""
		    ::cmdserver::broadcast $ST(servid) $response $sock
		}
	    }
	}
	REMOVE {
	    set svcs [list]
	    foreach id $remaining {
		set servid [::procwatch::find $id]
		if { $servid != "" } {
		    lappend svcs $servid
		}
	    }
	    set svcs [lsort -unique $svcs]
	    if { [llength $svcs] > 0 } {
		set killed [::procwatch::remove $svcs 1]
		::cmdserver::broadcast $ST(servid) "KILLED $killed" $sock
	    }
	}
	REMOVE_ALL {
	    set svcs [::procwatch::get $ST(pwid) services]
	    if { [llength $svcs] > 0 } {
		set killed [::procwatch::remove $svcs 1]
	    } else {
		set killed [list]
	    }
	    ::cmdserver::broadcast $ST(servid) "KILLED $killed" $sock
	}
	RESTART {
	    set svcs [list]
	    foreach id $remaining {
		set servid [::procwatch::find $id]
		if { $servid != "" } {
		    lappend svcs $servid
		}
	    }
	    set svcs [lsort -unique $svcs]
	    if { [llength $svcs] > 0 } {
		set killed [::procwatch::kill $svcs]
		#::procwatch::start $ST(pwid) $svcs
		::cmdserver::broadcast $ST(servid) "KILLED $killed" $sock
	    }
	}
	RESTART_ALL {
	    set svcs [::procwatch::get $ST(pwid) services]
	    if { [llength $svcs] > 0 } {
		set killed [::procwatch::kill $svcs]
		#::procwatch::start $ST(pwid) $svcs
	    } else {
		set killed [list]
	    }
	    ::cmdserver::broadcast $ST(servid) "KILLED $killed" $sock
	}
	RELOAD {
	    set svcs [::procwatch::get $ST(pwid) services]
	    if { [llength $svcs] > 0 } {
		set killed [::procwatch::remove $svcs 1]
	    } else {
		set killed [list]
	    }
	    foreach fname $ST(infiles) {
		foreach sid [::procwatch::read $ST(pwid) \
				 [::diskutil::fname_resolv $fname]] {
		    ::procwatch::config $sid -maxsize $ST(maxsize)
		}
	    }
	    ::cmdserver::broadcast $ST(servid) "KILLED $killed" $sock
	}
	WATCH {
	    foreach id $remaining {
		set servid [::procwatch::find $id]
		if { $servid ne "" } {
		    set already 0
		    foreach wid [::procwatch::get $servid logwatches] {
			set cmd [::procwatch::get $wid command]
			if { [lsearch $cmd $sock] >= 0 } {
			    set already 1
			}
		    }
		    if { ! $already } {
			set wid [::procwatch::log:watch $servid \
				     [list log_out $sock]]
		    }
		}
	    }
	}
	UNWATCH {
	    foreach id $remaining {
		set servid [::procwatch::find $id]
		if { $servid ne "" } {
		    ::procwatch::log:unwatch $servid
		}
	    }
	}
    }
}


# Guess the DNS server that can be used for address resolution on this
# very machine.  The code below should work on all platforms, since
# they all support nslookup (except MacOS <= 9).  If we do not manage,
# default to something here at SICS!
$ST(_log)::notice "Guessing DNS server address..."
set dns ""
set res [catch "::dnsresolv::init $ST(dns)" dns]
if { $res == 0 && $dns != "" } {
    $ST(_log)::info "Found as $dns"
} else {
    $ST(_log)::info "Not found, will have a hard time running!..."
}

set ST(pwid) [::procwatch::new -pause $ST(pause) -log $ST(log) -run $ST(run) \
		  -home $ST(home) -watch $ST(watch)]
foreach fname $ST(infiles) {
    foreach sid [::procwatch::read $ST(pwid) \
		     [::diskutil::fname_resolv $fname]] {
	::procwatch::config $sid -maxsize $ST(maxsize)
    }
}

# Start a command responding server.  Be sure to restrict incoming
# access to some hosts only.
set ST(servid) [::cmdserver::new $ST(port) \
		    [list LIST INFO REMOVE REMOVE_ALL RESTART RESTART_ALL \
			 RELOAD WATCH UNWATCH] incoming "Starter Server"]
if { $ST(servid) < 0} {
    $ST(_log)::error "Could not start server on $ST(port)"
    ::procwatch::remove "" 1
    exit
}
::cmdserver::assocs $ST(servid) "logwatch"
::cmdserver::restrict $ST(servid) $ST(allow) $ST(deny)

vwait $ST(finish)
