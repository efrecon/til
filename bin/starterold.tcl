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
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
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
#    infile	- File to initiate with on start up.
array set ST {
    servid         -1
    dns            "ns.sics.se"
    services       ""
    service_id     0
    finish         0
    wish           ""
    tclsh          ""
    schedule       ""
    java           ""
    jsearch_done   0
    svc_run_file   "running.%hostname%.svc"
    svc_pid_file   "starter.%hostname%.pid"
    tstamper       ""
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
    [list process cmdserver java logwatch diskutil] $ST(verbose)
package require struct::list

# Initialise local logging facility
package require logger
set ST(_log) [::logger::init starter]
$ST(_log)::setlevel $ST(verbose)

argutil::fix_outlog




# Command Name     --  runfile_write
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Write down a run description file that shortly describes the state
# of our currently running services.  Return the number of services
# for which information was written, -1 if the file could not be
# opened for writing.
#
# Arguments:
#    runfile	- Path to file, will be generated otherwise.
proc runfile_write { { runfile "" } } {
    global ST

    # Generate a valid and intelligent runfile name if none was
    # provided.
    if { $runfile == "" } {
	set runfile [::diskutil::fname_resolv \
			 [file join $ST(rundir) $ST(svc_run_file)]]
    }

    # And now write information for all services.
    $ST(_log)::info "Writing running state to $runfile"
    set nbservices 0
    if { [catch "open $runfile w" fd] == 0 } {
	foreach s $ST(services) {
	    set varname "__Service_$s"
	    upvar \#0 $varname Service
	    
	    if { $Service(pid) != "" } {
		puts $fd "$Service(id) \"$Service(pid)\" $Service(script)"
	    }
	    incr nbservices
	}
	close $fd
    } else {
	$ST(_log)::error "Failed writing running state to $runfile"
	return -1
    }
    
    return $nbservices
}


# Command Name     --  runfile_read
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Read a run description file that shortly describes the state of our
# previously currently running services.  Return a list describing
# these (empty list on no processes or error).
#
# Arguments:
#    runfile	- Path to file, will be generated otherwise.
proc runfile_read { { runfile "" } } {
    global ST

    set services ""

    # Generate a valid and intelligent runfile name if none was
    # provided.
    if { $runfile == "" } {
	set runfile [::diskutil::fname_resolv \
			 [file join $ST(rundir) $ST(svc_run_file)]]
    }

    # And now write information for all services.
    if { [catch "open $runfile r" fd] == 0 } {
	while { ! [eof $fd] } {
	    set line [gets $fd]
	    if { $line != "" } {
		set id [lindex $line 0]
		set pids [lindex $line 1]
		set script [lindex $line 2]
		lappend services [list $id $pids $script]
	    }
	}
	close $fd
    }
    
    return $services
}



# Command Name     --  service_find
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Find a service by its PID, return identifier or -1
#
# Arguments:
#    pid	- PID of service.
proc service_find { pid } {
    global ST

    foreach s $ST(services) {
	set varname "__Service_$s"
	upvar \#0 $varname Service

	if { [lindex $Service(pid) 0] == $pid } {
	    return $s
	}
    }

    return -1
}


# Command Name     --  services_read
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Read the configuration file.
#
# Arguments:
#    fname	- Name of file.
proc services_read { args } {
    global ST

    set svcs ""

    if { [llength $args] <= 0 } {
	set args $ST(infiles)
    }

    foreach fname $args {
	$ST(_log)::info "Reading services from: $fname"
	if { [catch "open $fname r" sf] != 0 } {
	    $ST(_log)::error "Unable to open $fname"
	} else {
	    while { ! [eof $sf] } {
		set line [string trim [gets $sf]]
		if { $line != "" } {
		    set firstchar [string index $line 0]
		    if { $firstchar == "\#" || $firstchar == ";" } {
		    } else {
			set script [lindex $line 1]
			set varname "__Service_$ST(service_id)"
			upvar \#0 $varname Service
			
			set Service(script) $script

			if { $ST(logdir) == "" } {
			    set Service(logfile) ""
			} else {
			    set raw \
				    [file tail \
				    [::diskutil::fname_resolv \
				    $ST(logfmt) $script]]
			    set Service(logfile) \
				    [file join $ST(logdir) $raw]
			}

			set exe [lindex $line 0]
			set install_it 0
			set test_existence 0
			if { [string match -nocase "t*" $exe] } {
			    set Service(exe) $ST(tclsh)
			    set test_existence 1
			    set install_it 1
			} elseif { [string match -nocase "w*" $exe] } {
			    set Service(exe) $ST(wish)
			    set test_existence 1
			    set install_it 1
			} elseif { [string match -nocase "j*" $exe] } {
			    if { ! $ST(jsearch_done) } {
				set ST(java) [::java::find vnumber]
				if { $ST(java) == "" } {
				    $ST(_log)::warn \
					    "Could not find java, ignoring!"
				} else {
				    $ST(_log)::notice \
					    "Using \"$ST(java)\" ($vnumber)\
					     for java programs"
				}
			    }
			    if { $ST(java) != "" } {
				set Service(exe) $ST(java)
				set install_it 1
			    }
			} elseif { [string match -nocase "s*" $exe] } {
			    set Service(exe) ""
			    set test_existence 1
			    set install_it 1
			}

			# Check if script actually points to an existing file
			if { $test_existence && ! [file exists $script] } {
			    $ST(_log)::warn \
				    "\"$script\" is not a readable\
				    file!"
			    set install_it 0
			}

			if { $install_it } {
			    set Service(id) $ST(service_id)
			    set Service(params) [lrange $line 2 end]
			    set Service(pid) ""
			    set Service(wid) ""

			    lappend ST(services) $ST(service_id)
			    lappend svcs $ST(service_id)
			    incr ST(service_id)
			} else {
			    $ST(_log)::warn "Could not install service \"$line\"!"
			    unset Service
			}
		    }
		}
	    }
	    close $sf
	}
    }

    return $svcs
}


# Command Name     --  services_start
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Start a number of services.  Do not start them if they are alread on.
# Return a list of the identifiers of the processes that were started.
#
# Arguments:
#    svcs	- List of services to start (empty means all)
#    force	- Force restarting of processes.
proc services_start { { svcs "" } { force 0 } } {
    global ST argv0

    if { $svcs == "" } {
	set svcs $ST(services)
    }

    set tstamper [file join [file dirname $argv0] timestamper.tcl]
    if { ! [file exists $tstamper] || ! [file readable $tstamper] } {
	set tstamper ""
    } else {
	if { [catch "::diskutil::absolute_path $tstamper" tstamper] != 0 } {
	    set tstamper ""
	}
    }
    
    set started ""
    foreach s $svcs {
	set varname "__Service_$s"
	upvar \#0 $varname Service

	if { $Service(pid) != "" && $force } {
	    foreach pid $Service(pid) {
		::process::kill $pid
	    }
	}

	if { $Service(pid) == "" || $force } {
	    if { $Service(logfile) != "" } {
		if { [catch "open $Service(logfile) a" lf] == 0 } {
		    puts $lf ""
		    puts $lf \
			"\tStarting service at [clock format [clock seconds]]"
		    puts $lf \
			"\t$Service(exe) $Service(script) $Service(params)"
		    close $lf
		} else {
		    $ST(_log)::warn "Cannot open log file $Service(logfile)"
		}
	    }
	    if { [catch "pwd" curdir] != 0 } {
		$ST(_log)::warn "Could not remember current directory!"
		set curdir ""
	    }
	    if { $Service(exe) == "" } {
		if { [catch "cd [file dirname $Service(script)]"] == 0 } {
		    set cmd "exec \"[file tail $Service(script)]\""
		} else {
		    $ST(_log)::error "Could not change directory to [file dirname $Service(script)]"
		    set cmd "exec \"$Service(script)\""
		}
		append cmd " $Service(params)"
	    } else {
		set cmd "exec \"$Service(exe)\""
		#set cmd "exec \"$Service(exe)\" $Service(script)"
		set idx [string first "-- " $Service(params)]
		if { $idx < 0 } {
		    append cmd " $Service(script) $Service(params)"
		} else {
		    append cmd \
			    " [string range $Service(params) 0 [expr $idx -1]]"
		    append cmd " $Service(script)"
		    append cmd " [string range $Service(params) [expr $idx + 3] end]"
		}
	    }
	    if { $Service(logfile) != "" } {
		if { $tstamper == "" } {
		    append cmd " >>& $Service(logfile)"
		} else {
		    append cmd " |& \"$ST(tclsh)\" \"$tstamper\" -ignorepreformat -datesensitive -outfile \"$Service(logfile)\""
		}
	    }
	    $ST(_log)::notice "Starting: $cmd"
	    append cmd " &"
	    set res [catch $cmd pid]
	    if { $res == 0 } {
		set Service(pid) $pid
		lappend started $s
		if { [llength $pid] == 1 } {
		    $ST(_log)::info "Started with pid: $pid"
		} else {
		    $ST(_log)::info "Started with pid: [lindex $pid 0] (logger: [lindex $pid 1])"
		}
	    } else {
		set Service(pid) ""
		$ST(_log)::warn "Could not start $cmd"
	    }
	    if { $curdir != "" } {
		if { [catch "cd $curdir"] != 0 } {
		    $ST(_log)::warn \
			"Could not change back to directory $curdir"
		}
	    }

	    if { $ST(rundir) != "" } {
		if { [runfile_write] < 0 } {
		    $ST(_log)::warn "Could not create running state file!"
		}
	    }
	    
	    after [expr $ST(pause) * 1000]
	}
    }

    return $started
}


# Command Name     --  services_find
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Find a service by its internal id (preferred), its current pid or its
# script name
#
# Arguments:
#    id 	- Internal id or pid.
proc services_find { id } {
    global ST

    set servid ""

    # First try to understand id and a service id
    if { [regexp "\\d+" $id] } {
	set idx [lsearch $ST(services) $id]
	if { $idx >= 0 } {
	    set servid [lindex $ST(services) $idx]
	}
    }

    # Otherwise try it as a pid, you never know...
    if { $servid == "" } {
	foreach sid $ST(services) {
	    set varname "__Service_$sid"
	    upvar \#0 $varname Service
	    if { [lindex $Service(pid) 0] == $id \
		     || [string match $id $Service(script)] } {
		set servid $sid
		break
	    }
	}
    }

    return $servid
}


# Command Name     --  __projection
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# This procedure returns the n'th index of a list.
#
# Arguments:
#    n  	- Index
#    list	- List to extract from
proc __projection { n list } {
    ::lindex $list $n
}

#------------------------------------------------------------------------------
# Copyright 1992-1999 Karl Lehenbauer and Mark Diekhans.
#
# Permission to use, copy, modify, and distribute this software and its
# documentation for any purpose and without fee is hereby granted, provided
# that the above copyright notice appear in all copies.  Karl Lehenbauer and
# Mark Diekhans make no representations about the suitability of this
# software for any purpose.  It is provided "as is" without express or
# implied warranty.
#------------------------------------------------------------------------------
#
# intersect3 - perform the intersecting of two lists, returning a list
# containing three lists.  The first list is everything in the first
# list that wasn't in the second, the second list contains the intersection
# of the two lists, the third list contains everything in the second list
# that wasn't in the first.
#
proc listutil_intersect3 {list1 list2} {
    set la1(0) {} ; unset la1(0)
    set lai(0) {} ; unset lai(0)
    set la2(0) {} ; unset la2(0)
    foreach v $list1 {
        set la1($v) {}
    }
    foreach v $list2 {
        set la2($v) {}
    }
    foreach elem [concat $list1 $list2] {
        if {[info exists la1($elem)] && [info exists la2($elem)]} {
            unset la1($elem)
            unset la2($elem)
            set lai($elem) {}
        }
    }
    list [lsort [array names la1]] [lsort [array names lai]] \
         [lsort [array names la2]]
}


# Command Name     --  services_watch
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Watch a number of (started) services.  Restart them if necessary.
#
# Arguments:
#    svcs	- List of services to start (empty means all)
proc services_watch { { svcs "" } } {
    global ST

    if { $svcs == "" } {
	set svc_watch $ST(services)
    } else {
	set svc_watch $svcs
    }

    set svc_pids ""
    foreach s $svc_watch {
	set varname "__Service_$s"
	upvar \#0 $varname Service
	if { $Service(pid) != "" } {
	    lappend svc_pids [lindex $Service(pid) 0]
	}
    }

    if { [catch "::process::full_list" f_pids] == 0 } {
	# First restart processes that have disappeared and crashed
	set pids [::struct::list map $f_pids "__projection 0"]
	set isect [listutil_intersect3 $pids $svc_pids]
	set died [lindex $isect 2]
	if { $died != "" } {
	    set restart ""
	    foreach d $died {
		set id [service_find $d]
		if { $id >= 0 } {
		    lappend restart $id
		}
	    }
	    $ST(_log)::notice "Some services have died, restarting: $restart"
	    foreach r $restart {
		set varname "__Service_$r"
		upvar \#0 $varname Service
		if { [llength $Service(pid)] > 1 } {
		    ::process::kill [lindex $Service(pid) 1]
		}
		set Service(pid) ""
	    }
	    services_start $restart 1
	}

	# Now, restart processes that have grown too big.
	if { $ST(maxsize) > 0 } {
	    set restart ""
	    foreach p_info $f_pids {
		set msize [lindex $p_info 4]
		if { $msize > $ST(maxsize) } {
		    set pid [lindex $p_info 0]
		    set id [service_find $pid]
		    if { $id >= 0 } {
			lappend restart $id
		    }
		}
	    }
	    
	    if { [llength $restart] > 0 } {
		$ST(_log)::notice \
		    "Some services are too big, restarting: $restart"
		services_start $restart 1
	    }
	}
    } else {
	$ST(_log)::warn "Cannot get process list for now, sleeping again"
    }

    set ST(schedule) [after [expr $ST(watch) * 1000] \
			  "services_watch \"$svcs\""]
}


# Command Name     --  services_kill
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Force killing of some or all of the current processes associated to
# the services.  This procedure does not restart these processes.
#
# Arguments:
#    svcs	- List of services to kill (empty means all)
proc services_kill { { svcs "" } } {
    global ST

    if { $svcs == "" } {
	set svcs $ST(services)
    }

    set killed ""

    foreach s $svcs {
	set varname "__Service_$s"
	upvar \#0 $varname Service

	if { $Service(pid) != "" } {
	    foreach pid $Service(pid) {
		if { [llength [::process::kill $pid]] > 0 } {
		    lappend killed $pid
		}
	    }
	    set Service(pid) ""
	}

	if { $ST(rundir) != "" } {
	    if { [runfile_write] < 0 } {
		$ST(_log)::warn "Could not create running state file"
	    }
	}
    }

    return $killed
}


# Command Name     --  services_remove
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Remove a list of services, kill them if force is on.  Return the
# list of processes that were killed.
#
# Arguments:
#    svcs	- List of services to remove (empty means all)
#    force	- Force killing of processes.
proc services_remove { { svcs "" } { force 0 } } {
    global ST

    if { $svcs == "" } {
	set svcs $ST(services)
    }

    set killed ""

    foreach s $svcs {
	set varname "__Service_$s"
	upvar \#0 $varname Service

	if { $Service(pid) != "" && $force } {
	    foreach pid $Service(pid) {
		if { [llength [::process::kill $pid]] > 0 } {
		    $ST(_log)::debug "Killed processes associated to service $s: $pid"
		    lappend killed $pid
		} else {
		    $ST(_log)::info "Could not kill process $pid"
		}
	    }
	}

	set idx [lsearch $ST(services) $Service(id)]
	set ST(services) [lreplace $ST(services) $idx $idx]

	if { $Service(wid) != "" } {
	    ::logwatch::delete $Service(wid)
	}
	unset Service

	if { $ST(rundir) != "" } {
	    if { [runfile_write] < 0 } {
		$ST(_log)::warn "Could not create running state file"
	    }
	}
    }

    return $killed
}


# Command Name     --  log_out
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Command called back when log lines have been detected.
#
# Arguments:
#    wid	- Identifier of watch
#    line	- Line of log
proc log_out { wid line } {
    global ST

    set found 0
    foreach s $ST(services) {
	set varname "__Service_$s"
	upvar \#0 $varname Service
	if { $wid == $Service(wid) } {
	    set found 1
	    break
	}
    }

    if { $found } {
	set socks [::cmdserver::clients $ST(servid)]
	foreach s $socks {
	    set watchs [::cmdserver::assoc_get $s logwatch]
	    set idx [lsearch $watchs $wid]
	    if { $idx >= 0 } {
		puts $s "LOG $Service(id) $line"
		flush $s
	    }
	}
    }
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
	    set svcs ""
	    foreach sid $ST(services) {
		set varname "__Service_$sid"
		upvar \#0 $varname Service
		lappend svcs $Service(id) [lindex $Service(pid) 0]
	    }
	    puts $sock "LIST $svcs"
	    flush $sock
	}
	INFO {
	    foreach id $remaining {
		set servid [services_find $id]
		if { $servid != "" } {
		    set varname "__Service_$servid"
		    upvar \#0 $varname Service
		    puts $sock "INFO $Service(id) \"$Service(pid)\" $Service(logfile) $Service(exe) $Service(script) $Service(params)"
		    flush $sock
		}
	    }
	}
	REMOVE {
	    set svcs ""
	    foreach id $remaining {
		set servid [services_find $id]
		if { $servid != "" } {
		    lappend svcs $servid
		}
	    }
	    set svcs [lsort -unique $svcs]
	    if { $svcs != "" } {
		after cancel $ST(schedule)
		set killed [services_remove $svcs 1]
		set ST(schedule) [after [expr $ST(watch) * 1000] \
				      "services_watch"]
		puts $sock "KILLED $killed"
		flush $sock
	    }
	}
	REMOVE_ALL {
	    after cancel $ST(schedule)
	    set killed [services_remove "" 1]
	    set ST(schedule) [after [expr $ST(watch) * 1000] \
				  "services_watch"]
	    puts $sock "KILLED $killed"
	    flush $sock
	}
	RESTART {
	    set svcs ""
	    foreach id $remaining {
		set servid [services_find $id]
		if { $servid != "" } {
		    lappend svcs $servid
		}
	    }
	    set svcs [lsort -unique $svcs]
	    if { $svcs != "" } {
		after cancel $ST(schedule)
		set killed [services_kill $svcs]
		services_start $svcs 0
		set ST(schedule) [after [expr $ST(watch) * 1000] \
				      "services_watch"]
		puts $sock "KILLED $killed"
		flush $sock
	    }
	}
	RESTART_ALL {
	    after cancel $ST(schedule)
	    set killed [services_kill ""]
	    services_start "" 0
	    set ST(schedule) [after [expr $ST(watch) * 1000] \
				  "services_watch"]
	    puts $sock "KILLED $killed"
	    flush $sock
	}
	RELOAD {
	    after cancel $ST(schedule)
	    set killed [services_remove "" 1]
	    services_start [services_read] 1
	    set ST(schedule) [after [expr $ST(watch) * 1000] \
				  "services_watch"]
	    puts $sock "KILLED $killed"
	    flush $sock
	}
	WATCH {
	    foreach id $remaining {
		set servid [services_find $id]
		if { $servid != "" } {
		    set varname "__Service_$servid"
		    upvar \#0 $varname Service
		    
		    # Start a log watch on the appropriate file if necessary
		    if { $Service(wid) == "" && $Service(logfile) != "" } {
			set Service(wid) \
			    [::logwatch::new $Service(logfile) log_out]
		    }

		    # Remember that we want to get log information
		    if { $Service(wid) != "" } {
			set watchs [::cmdserver::assoc_get $sock logwatch]
			set idx [lsearch $watchs $Service(wid)]
			if { $idx < 0 } {
			    lappend watchs $Service(wid)
			    ::cmdserver::assoc_put $sock logwatch $watchs
			}
		    }
		}
	    }
	}
	UNWATCH {
	    foreach id $remaining {
		set servid [services_find $id]
		if { $servid != "" } {
		    set varname "__Service_$servid"
		    upvar \#0 $varname Service
		    
		    # Remember that we want to get log information
		    if { $Service(wid) != "" } {
			set watchs [::cmdserver::assoc_get $sock logwatch]
			set idx [lsearch $watchs $Service(wid)]
			if { $idx >= 0 } {
			    set watchs [lreplace $watchs $idx $idx]
			    ::cmdserver::assoc_put $sock logwatch $watchs
			}
		    }
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


# Create run directory if necessary.  Disable running facilities if we
# could not create it.
set ST(rundir) ""
if { $ST(run) != "" } {
    set ST(rundir) [file join [::diskutil::fname_resolv $ST(home)] $ST(run)]
    if { ! [file exists $ST(rundir)] } {
	$ST(_log)::notice "Creating directory $ST(rundir)"
	file mkdir $ST(rundir)
    } elseif { ! [file isdirectory $ST(rundir)] } {
	$ST(_log)::warn "Run directory $ST(rundir) is not a directory!"
	set ST(rundir) ""
    }
    if { $ST(rundir) != "" } {
	set ST(rundir) [::diskutil::absolute_path $ST(rundir)]
    }
}
    

# Create log directory if necessary.  Disable logging facilities if we
# could not create it.
set ST(logdir) ""
if { $ST(log) != "" } {
    set ST(logdir) [file join [::diskutil::fname_resolv $ST(home)] $ST(log)]
    if { ! [file exists $ST(logdir)] } {
	$ST(_log)::notice "Creating logging directory $ST(logdir)"
	file mkdir $ST(logdir)
    } elseif { ! [file isdirectory $ST(logdir)] } {
	$ST(_log)::warn "Logging directory $ST(logdir) is not a directory!"
	set ST(logdir) ""
    }
    if { $ST(logdir) != "" } {
	set ST(logdir) [::diskutil::absolute_path $ST(logdir)]
    }
}
    

# Now create a pidfile for ourselves and make sure that no other
# instance of ourselves is already running (otherwise, we would have
# chaotic behaviour!)
if { $ST(rundir) != "" } {
    # Refuse to start if another starter is already running on that host
    set pidfile [::diskutil::fname_resolv \
		     [file join $ST(rundir) $ST(svc_pid_file)]]
    if { [catch "open $pidfile" fd] == 0 } {
	set pid [gets $fd]
	close $fd

	set rx "[file tail [info nameofexecutable]]\\s+.*[file tail $argv0]"
	set other_progs [::process::find $rx]
	if { [lsearch $other_progs $pid] >= 0 } {
	    $ST(_log)::warn \
		"Another [file tail $argv0] already running at $pid!"
	    exit 1
	}
    }

    # Now writing pidfile
    $ST(_log)::info "Writing pid file: $pidfile"
    set fd [open $pidfile w]
    puts $fd [pid]
    close $fd
}


# Performs substitution on services files.
set ST(infiles) [::diskutil::fname_resolv $ST(infiles)]


# Guess good executables for tclsh and wish, these are generated from
# the script that actually started us and from the same directory.
set exedir [file dirname [info nameofexecutable]]
set ext [file extension [info nameofexecutable]]
set raw [file rootname [file tail [info nameofexecutable]]]
set version ""
if { [regexp "\\w+\\d+" $raw] } {
    set len [string length $raw]
    for { set vidx 0 } { $vidx < $len } { incr vidx } {
	if { [string is digit [string index $raw $vidx]] } {
	    break
	}
    }
    set version [string range $raw $vidx end]
}
set ST(tclsh) [file join $exedir "tclsh${version}${ext}"]
set ST(wish) [file join $exedir "wish${version}${ext}"]

$ST(_log)::info "Using \"$ST(tclsh)\" for tcl-only scripts"
$ST(_log)::info "Using \"$ST(wish)\" for tk scripts"


# Recapture possibly running services that we had started during a
# previous session before we died for some unknown reason or run
# directly all the services that are pointed at by the files.
if { $ST(run) == "" } {
    services_start [services_read]
} else {
    # Recapturing works as follows: The current service descriptions
    # are read.  Then the last running state is read.  Finally, a
    # comparison is made between those.  This comparison is based on
    # the current list of running processes at startup.
    $ST(_log)::notice "Recapturing possibly lost services..."
    set services [services_read]
    set running [runfile_read]
    set all_pids [::process::list]

    # Now, for all old processes in the last running state, see if
    # these are still running (i.e. one of all_pids variable) and that
    # the running processes actually match the name of the script
    # identifying the process (anything else can have been starting
    # inbetween runs and got the same pid!).
    foreach s_info $running {
	set s_pids [lindex $s_info 1]
	set found 0
	if { [lsearch $all_pids [lindex $s_pids 0]] >= 0 } {
	    set m_pids [::process::find [lindex $s_info 2]]
	    if { [lsearch $m_pids [lindex $s_pids 0]] >= 0 } {
		set found 1
	    }
	}
	if { $found && [llength $s_pids] > 1 } {
	    set found 0
	    if { [lsearch $all_pids [lindex $s_pids 1]] >= 0 } {
		set m_pids [::process::find timestamper.tcl]
		if { [lsearch $m_pids [lindex $s_pids 1]] >= 0 } {
		    set found 1
		}
	    }
	}
	
	# We have found and matched both old pids against the actual
	# running state.  Recapture through setting the Service(pid)
	# list back to these pids.
	if { $found } {
	    $ST(_log)::info "Recaptured [lindex $s_info 2]..."
	    set id [services_find [lindex $s_info 2]]
	    if { $id >= 0 } {
		set varname "__Service_$id"
		upvar \#0 $varname Service
		set Service(pid) $s_pids
	    }
	}
    }

    # Now start services.  Those that were recaptured will not be
    # started again since we call services_start without the force
    # option.
    services_start $services
}


# Start a command responding server.  Be sure to restrict incoming
# access to some hosts only.
set ST(servid) [::cmdserver::new $ST(port) \
		    [list LIST INFO REMOVE REMOVE_ALL RESTART RESTART_ALL \
			 RELOAD WATCH UNWATCH] incoming "Starter Server"]
if { $ST(servid) < 0} {
    $ST(_log)::error "Could not start server on $ST(port)"
    services_remove "" 1
    exit
}
::cmdserver::assocs $ST(servid) "logwatch"
::cmdserver::restrict $ST(servid) $ST(allow) $ST(deny)
set ST(schedule) [after idle services_watch]

vwait $ST(finish)
