# procwatch.tcl -- Service controller support
#
#	This module provides all the support mechanism for starting a
#	number of processes and ensuring that they will continuously
#	run at any time.  The module provides a number of heuristics
#	for restarting badly behaved processes.  It will also
#	continuously log the output of processes to files.
#
# Copyright (c) 2004-2007 by the Swedish Institute of Computer Science.
#
# See the file 'license.terms' for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

package require Tcl 8.4
package require logger
package require struct::list

package require uobj
package require process
package require java
package require logwatch
package require diskutil

namespace eval ::procwatch {
    variable PW
    if {![::info exists PW]} {
	array set PW {
	    cidgene           0
	    sidgene           0
	    widgene           0
	    vm_tclsh          ""
	    vm_wish           ""
	    vm_java           ""
	    comments          "\#!;"
	    svc_run_file      "running.%hostname%.%context%.svc"
	    svc_pid_file      "context.%hostname%.%context%.pid"
	    -logfmt           "%progname%.%hostname%.log"
	    -log              "log"
	    -run              "run"
	    -uniqueness       "::procwatch::__exit"
	    -maxsize          65536
	    -autostart        on
	    -home             "%progdir%"
	    -pause            1
	    -watch            10
	    -type             ""
	    -main             ""
	    -vmargs           ""
	    -args             ""
	    cx_inheritance    "-log -run -home -pause -uniqueness -watch -autostart"
	    srv_inheritance   "-logfmt -maxsize -type -main -vmargs -args"
	}
	variable libdir [file dirname [file normalize [info script]]]
	::uobj::install_log procwatch PW; # Creates log namespace variable
	::uobj::install_defaults procwatch PW; # Creates defaults procedure
    }
}

# This module handles two different types of "objects": Contexts and
# Services.  Contexts provides the resources for the controlling of a
# number of processes, this includes things such as location for files
# on disk.  Processes that should be watched are added to contexts and
# are named Services, since they should be kept running at all time
# and under any circumstances.


# ::procwatch::isa -- Detect object type
#
#	Detect if an object is of a given type, possibly returning
#	back it creation iteration identifier.
#
# Arguments:
#	id	Identifier of object
#	type	type that we are requesting for
#	iter_p	Pointer to iteration creation identifier
#
# Results:
#	1 if object is of correct type, 0 otherwise
#
# Side Effects:
#	None.
proc ::procwatch::isa { id type {iter_p ""} } {
    variable PW
    variable log

    if { $iter_p ne "" } {
	upvar $iter_p creationid
    }
    set creationid -1
    if { [string match "[namespace current]::${type}_*" $id] } {
	set uscore [string last _ $id]
	set creationid [string range $id [expr $uscore +1] end]
	if { [info vars $id] ne "" } {
	    return 1
	} else {
	    return 0
	}
    }
    return 0
}


# ::procwatch::__initdirs -- Create directories
#
#	This procedure will create the resource directories needed for
#	the run-time controlling of services within a context.
#
# Arguments:
#	id	Identifier of a context, as returned by ::procwatch::new
#
# Results:
#	None.
#
# Side Effects:
#	Creates logging and run control directories if necessary.
#	Turn off facilities on errors.
proc ::procwatch::__initdirs { id } {
    variable PW
    variable log
    
    upvar #0 $id CONTEXT

    # Create run directory if necessary.  Disable running facilities
    # if we could not create it.
    set CONTEXT(rundir) ""
    if { $CONTEXT(-run) ne "" } {
	${log}::notice "Turning on run control facilities"
	set CONTEXT(rundir) \
	    [file join [::diskutil::fname_resolv $CONTEXT(-home)] \
		 $CONTEXT(-run)]
	if { ! [file exists $CONTEXT(rundir)] } {
	    ${log}::notice "Creating directory $CONTEXT(rundir)"
	    file mkdir $CONTEXT(rundir)
	} elseif { ! [file isdirectory $CONTEXT(rundir)] } {
	    ${log}::warn "Run directory $CONTEXT(rundir) is not a directory!"
	    set CONTEXT(rundir) ""
	}

	if { $CONTEXT(rundir) != "" } {
	    set CONTEXT(rundir) [::diskutil::absolute_path $CONTEXT(rundir)]
	}
    }

    # Create log directory if necessary.  Disable logging facilities
    # if we could not create it.
    set CONTEXT(logdir) ""
    if { $CONTEXT(-log) != "" } {
	${log}::notice "Turning on logging facilities"
	set CONTEXT(logdir) \
	    [file join [::diskutil::fname_resolv $CONTEXT(-home)] \
		 $CONTEXT(-log)]
	if { ! [file exists $CONTEXT(logdir)] } {
	    ${log}::notice "Creating logging directory $CONTEXT(logdir)"
	    file mkdir $CONTEXT(logdir)
	} elseif { ! [file isdirectory $CONTEXT(logdir)] } {
	    ${log}::warn \
		"Logging directory $CONTEXT(logdir) is not a directory!"
	    set CONTEXT(logdir) ""
	}
	if { $CONTEXT(logdir) != "" } {
	    set CONTEXT(logdir) [::diskutil::absolute_path $CONTEXT(logdir)]
	}
    }
}


# ::procwatch::__exit -- Exit on duplicate controllers
#
#	This procedure is the default behaviour when the same context
#	is controlled by another process.  It will exit this process
#	after printing out an error.
#
# Arguments:
#	pid	Process identifier of the other controlling process
#
# Results:
#	None.
#
# Side Effects:
#	Will actively terminate the current process
proc ::procwatch::__exit { pid } {
    variable PW
    variable log

    ${log}::critical "Another process at $pid is already in control of\
                      this context, exiting"
    exit 1
}


# ::procwatch::__checkuniqueness -- Scream/exit when non-unique
#
#	This procedure tries to ensure that a single process, on that
#	host, is controlling a given context.  If several were, things
#	would end into chaos (since we would attempt to start several
#	times the same service).
#
# Arguments:
#	id	Identifier of the context, as returned by ::procwatch::new
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::procwatch::__uniqueness { id } {
    variable PW
    variable log
    
    upvar #0 $id CONTEXT

    # Now create a pidfile for ourselves and make sure that no other
    # instance of ourselves is already running (otherwise, we would have
    # chaotic behaviour!)
    if { $CONTEXT(rundir) != "" && $CONTEXT(-uniqueness) ne "" } {
	isa $id context cxid
	# Refuse to start if another starter is already running on that host
	set pidfile [::diskutil::fname_resolv \
			 [file join $CONTEXT(rundir) $PW(svc_pid_file)]]
	set pidfile [regsub -all "%context%" $pidfile $cxid]
	if { [catch "open $pidfile" fd] == 0 } {
	    set pid [gets $fd]
	    close $fd

	    # Search for processes which have the same name as
	    # executable (which will work both if we are a wrapped
	    # script or a script started from wish or tclsh).
	    set rx "[file tail [info nameofexecutable]]"
	    set other_progs [::process::find $rx]
	    if { [lsearch $other_progs $pid] >= 0 && $pid != [pid]} {
		${log}::error "Another process is already controlling\
                               context $id at $pid!"
		if { [catch {eval $CONTEXT(-uniqueness) $pid} err] } {
		    ${log}::error \
			"Error when calling $CONTEXT(-uniqueness): $err"
		}
	    }
	}
	
	# Now writing pidfile
	${log}::info "Writing pid file: $pidfile"
	set fd [open $pidfile w]
	puts $fd [pid]
	close $fd
    }
}


# ::procwatch::runfile:write -- Write run-time information file
#
#       Write down a run description file that shortly describes the
#       state of the services currently running in a context.  
#
# Arguments:
#	id	Identifier of the context
#	runfile	Path to file, will be generated otherwise
#
# Results:
#       Return the number of services for which information was written, -1
#       if the file could not be opened for writing.
#
# Side Effects:
#	None.
proc ::procwatch::runfile:write { id {runfile ""} } {
    variable PW
    variable log

    if { ![isa $id context cxid] } {
	${log}::warn "$id is not a known context"
	return -code error "$id is not a known context"
    }

    upvar #0 $id CONTEXT
    # Generate a valid and intelligent runfile name if none was
    # provided.
    if { $runfile == "" } {
	set runfile [::diskutil::fname_resolv \
			 [file join $CONTEXT(rundir) $PW(svc_run_file)]]
	set runfile [regsub -all "%context%" $runfile $cxid]
    }

    # And now write information for all services.
    ${log}::info "Writing running state to $runfile"
    set nbservices 0
    if { [catch "open $runfile w" fd] == 0 } {
	foreach s [get $id services] {
	    upvar \#0 $s SERVICE
	    
	    if { $SERVICE(state) eq "RUNNING" && [llength $SERVICE(pid)] > 0 } {
		puts $fd "$SERVICE(id) \"$SERVICE(pid)\" $SERVICE(-main)"
	    }
	    incr nbservices
	}
	close $fd
    } else {
	${log}::error "Failed writing running state to $runfile"
	return -1
    }
    
    return $nbservices
}


# ::procwatch::runfile:read -- Read run-time information file
#
#       Write down a run description file that shortly describes the
#       state of the services currently running in a context.  
#
# Arguments:
#	id	Identifier of the context
#	runfile	Path to file, will be generated otherwise
#
# Results:
#       Return a list describing these (empty list on no processes or error).
#
# Side Effects:
#	None.
proc ::procwatch::runfile:read { id {runfile ""} } {
    variable PW
    variable log

    if { ![isa $id context cxid] } {
	${log}::warn "$id is not a known context"
	return -code error "$id is not a known context"
    }

    upvar #0 $id CONTEXT
    # Generate a valid and intelligent runfile name if none was
    # provided.
    if { $runfile == "" } {
	set runfile [::diskutil::fname_resolv \
			 [file join $CONTEXT(rundir) $PW(svc_run_file)]]
	set runfile [regsub -all "%context%" $runfile $cxid]
    }

    # And now read information for all services.
    set services [list]
    if { [catch "open $runfile r" fd] == 0 } {
	while { ! [eof $fd] } {
	    set line [gets $fd]
	    if { $line != "" } {
		foreach {id pids script} $line break
		lappend services [list $id $pids $script]
	    }
	}
	close $fd
    }
    
    return $services
}


# ::procwatch::find -- Find a service
#
#	Find a service by its PID.
#
# Arguments:
#	pid	PID of service
#
# Results:
#	Return the identifier of the service or -1
#
# Side Effects:
#	None.
proc ::procwatch::find:bypid { pid } {
    variable PW
    variable log

    foreach v [info vars ::procwatch::service_*] {
	upvar #0 $v SERVICE
	if { [lindex $SERVICE(pid) 0] == $pid } {
	    return $v
	}
    }

    return ""
}


# ::procwatch::find -- Find a service
#
#	Find a service by its internal id (preferred), its current pid
#	or its script name
#
# Arguments:
#	id	Internal id, pid or name
#
# Results:
#	Return the identifer of the service, empty when not found
#
# Side Effects:
#	None.
proc ::procwatch::find { id } {
    variable PW
    variable log

    if { [isa $id service] } {
	return $id
    }

    set servid ""

    # First try to understand id as an internal service id
    if { [regexp "\\d+" $id] } {
	set vname [info vars ::procwatch::service_*_$id]
	if { $vname ne "" } {
	    return $vname
	}
    }

    # Next get on with name
    foreach v [info vars ::procwatch::service_*] {
	upvar #0 $v SERVICE
	if { [lindex $SERVICE(pid) 0] == $id \
		 || [string match $id $SERVICE(-main)] } {
	    return $v
	}
    }
    
    return ""
}


# ::procwatch::__vmfind -- Find a Virtual Machine
#
#	This procedure finds a virtual machine of a given type.  The
#	results are cached for quickening further lookups.  Recognised
#	virtual machines are tclsh, wish and java.
#
# Arguments:
#	type	Type of the virtual machine that we are looking for
#
# Results:
#	Return the full path to the execution binary for the VM, or an
#	empty strings on errors.
#
# Side Effects:
#	None.
proc ::procwatch::__vmfind { type } {
    variable PW
    variable log

    # Initialise the tclsh/wish cache.
    if { $PW(vm_tclsh) eq "" } {
	set starter [file tail [info nameofexecutable]]
	if { [string match wish* $starter] \
		 || [string match tclsh* $starter] } {
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
	    set PW(vm_tclsh) [file join $exedir "tclsh${version}${ext}"]
	    set PW(vm_wish) [file join $exedir "wish${version}${ext}"]
	} else {
	    set PW(vm_tclsh) [auto_execok tclsh]
	    set PW(vm_wish) [auto_execok wish]
	}

	if { ![file executable $PW(vm_tclsh)] } {
	    ${log}::warn "$PW(vm_tclsh) is not a valid tclsh VM!"
	    set PW(vm_tclsh) ""
	} else {
	    ${log}::info "Using \"$PW(vm_tclsh)\" for tcl-only scripts"
	}
	if { ![file executable $PW(vm_wish)] } {
	    ${log}::warn "$PW(vm_wish) is not a valid wish VM!"
	    set PW(vm_wish) ""
	} else {
	    ${log}::info "Using \"$PW(vm_wish)\" for tk scripts"
	}
    }

    # Now return the location of the requested VM or an empty string
    # on errors.
    set type [string tolower $type]
    switch -glob -- $type {
	t* {
	    return $PW(vm_tclsh)
	}
	w* {
	    return $PW(vm_wish)
	}
	j* {
	    if { $PW(vm_java) eq "" } {
		set PW(vm_java) [::java::find vnumber]
		if { $PW(vm_java) eq "" } {
		    ${log}::warn "Could not find java"
		} else {
		    ${log}::notice "Using \"$PW(vm_java)\" ($vnumber)\
				    for java programs"
		}
	    }
	    return $PW(vm_java)
	}
    }
    return ""
}


# ::procwatch::__resolv -- Resolve service run-time support
#
#	This procedure initialises the necessary variables that will
#	be essential to the run-time support of a service: where to
#	log, what to start, where to find VM interpreters (tcl, java,
#	etc.), etc.
#
# Arguments:
#	id	Identifier of a service, as returned by ::procwatch::add
#
# Results:
#	Return a boolean value telling if the script should be
#	installed and started or not.
#
# Side Effects:
#	None.
proc ::procwatch::__resolv { id } {
    variable PW
    variable log
    
    upvar #0 $id SERVICE
    upvar #0 $SERVICE(context) CONTEXT
    
    # Decide where we should log and whether we should log or not.
    set SERVICE(logfile) ""
    if { $CONTEXT(logdir) ne "" } {
	set raw [file tail [::diskutil::fname_resolv \
				$SERVICE(-logfmt) $SERVICE(-main)]]
	set SERVICE(logfile) [file join $CONTEXT(logdir) $raw]
    }

    # Decide which executable to start
    set exe [string tolower $SERVICE(-type)]
    set install_it 0
    set test_existence 0
    switch -glob -- $exe {
	t* {
	    set SERVICE(exe) [__vmfind tclsh]
	    set test_existence 1
	    set install_it 1
	}
	w* {
	    set SERVICE(exe) [__vmfind wish]
	    set test_existence 1
	    set install_it 1
	}
	j* {
	    set SERVICE(exe) [__vmfind java]
	    set install_it 1
	}
	s* -
	e* -
	b* {
	    set SERVICE(exe) ""
	    set test_existence 1
	    set install_it 1
	}
    }

    # Check if script actually points to an existing file
    if { $test_existence && ! [file exists $SERVICE(-main)] } {
	${log}::warn "\"$SERVICE(-main)\" is not a readable file!"
	set install_it 0
    }

    if { $install_it } {
	set SERVICE(pid) ""
    } else {
	set SERVICE(state) DISABLED
    }

    return $install_it
}


# ::procwatch::__buildcmd -- Build command
#
#	This procedure builds and remember the command necessary to
#	start a service.
#
# Arguments:
#	id	Identifier of the service.
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::procwatch::__buildcmd { id } {
    variable PW
    variable log
    variable libdir
    
    # Look for the official script used for timestamping output
    set tstamper [file join $libdir .. bin timestamper.tcl]
    if { ! [file exists $tstamper] || ! [file readable $tstamper] } {
	set tstamper ""
    } else {
	if { [catch "::diskutil::absolute_path $tstamper" tstamper] != 0 } {
	    set tstamper ""
	}
    }

    # Decide upon the command to start the service, this is still in
    # pseudo-code: %dir% will be replaced by the directory of the
    # script or by an empty directory if it is possible to change to
    # SERVICE(cmddir).
    upvar #0 $id SERVICE
    set SERVICE(cmddir) [file dirname $SERVICE(-main)]
    if { $SERVICE(exe) == "" } {
	set SERVICE(cmd) "exec \"%dir%[file tail $SERVICE(-main)]\""
	append SERVICE(cmd) " $SERVICE(-args)"
    } else {
	set SERVICE(cmd) "exec \"$SERVICE(exe)\""
	append SERVICE(cmd) " $SERVICE(-vmargs)"
	append SERVICE(cmd) " \"%dir%[file tail $SERVICE(-main)]\""
	append SERVICE(cmd) " $SERVICE(-args)"
    }

    # Append log dump information if necessary.
    if { $SERVICE(logfile) ne "" } {
	set tclsh [__vmfind tclsh]
	if { $tstamper == "" || $tclsh eq "" } {
	    set SERVICE(timestamper) ""
	    append SERVICE(cmd) " >>& $SERVICE(logfile)"
	} else {
	    set SERVICE(timestamper) "\"$tclsh\" \"$tstamper\" -ignorepreformat -datesensitive -outfile \"$SERVICE(logfile)\""
	    append SERVICE(cmd) " |& $SERVICE(timestamper)"
	}
    }

    ${log}::debug "Built pseudo-command for service $id: $SERVICE(cmd)"

    return $SERVICE(cmd)
}


# ::procwatch::__exec -- Execute service command
#
#	This procedure executes the command that should be used for
#	starting the service.
#
# Arguments:
#	id	Identifier of the service.
#
# Results:
#	Return the list of pids for the service, empty on errors.
#
# Side Effects:
#	Will spawn one or more processes!
proc ::procwatch::__exec { id } {
    variable PW
    variable log

    upvar #0 $id SERVICE
    upvar #0 $SERVICE(context) CONTEXT

    # Build command if it does not exist.
    if { $SERVICE(cmd) eq "" } {
	__buildcmd $id
    }

    # Append restarting header with time to logfile.
    if { $SERVICE(logfile) != "" } {
	if { [catch "open $SERVICE(logfile) a" lf] == 0 } {
	    puts $lf ""
	    puts $lf \
		"\tStarting service at [clock format [clock seconds]]"
	    puts $lf \
		"\t$SERVICE(exe) $SERVICE(-main) $SERVICE(-args)"
	    close $lf
	} else {
	    $ST(_log)::warn "Cannot open log file $SERVICE(logfile)"
	}
    }

    # Remember which directory we are in and change to the appropriate
    # directory, if possible.  Try to adapt if we could not.
    if { [catch "pwd" curdir] != 0 } {
	$ST(_log)::warn "Could not remember current directory!"
	set curdir ""
    }

    if { [catch "cd $SERVICE(cmddir)"] == 0 } {
	set cmd [regsub %dir% $SERVICE(cmd) ""]
    } else {
	set cmd [regsub %dir% $SERVICE(cmd) "$SERVICE(cmddir)/"]
    }

    # Start the service
    ${log}::notice "Spawning: $cmd"
    append cmd " &"
    set res [catch $cmd pid]
    if { $res == 0 } {
	set SERVICE(pid) $pid
	set SERVICE(state) RUNNING
	if { [llength $pid] == 1 } {
	    if { $SERVICE(timestamper) ne "" } {
		${log}::info "Could not start service correctly, aborting"
		::process::kill $pid
		set SERVICE(pid) $pid
		set SERVICE(state) DEAD
	    } else {
		${log}::info "Started with pid: $pid"
	    }
	} else {
	    ${log}::info "Started with pid: [lindex $pid 0]\
                          (logger: [lindex $pid 1])"
	}
    } else {
	set SERVICE(pid) ""
	set SERVICE(state) DEAD
	${log}::warn "Could not start $cmd"
    }

    # Change back to the starting directory
    if { $curdir ne "" } {
	if { [catch "cd $curdir"] != 0 } {
	    ${log}::warn "Could not change back to directory $curdir"
	}
    }

    # Remember the state of the current context.
    if { $CONTEXT(rundir) ne "" } {
	if { [runfile:write $CONTEXT(id)] < 0 } {
	    ${log}::warn "Could not create running state file!"
	}
    }
    
    return $SERVICE(pid)
}


# ::procwatch::__recapture -- Recapture running processes
#
#	Recapture already running processes into the services of a
#	context.  The last running state is read and a comparison is
#	made between those.  This comparison is based on the current
#	list of running processes at startup.
#
# Arguments:
#	cx	Identifier of context
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::procwatch::__recapture { cx } {
    variable PW
    variable log

    # Reject if wrong context
    if { ![isa $cx context] } {
	${log}::warn "$cx is not a known context"
	return -code error "$cx is not a known context"
    }

    # Skip recapturing if the context does not have any running
    # directory for process information storage.
    upvar #0 $cx CONTEXT
    if { $CONTEXT(rundir) eq "" } {
	return
    }

    ${log}::notice "Recapturing possibly lost services..."
    set running [runfile:read $cx]
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
	    ${log}::info "Recaptured [lindex $s_info 2]..."
	    set id [find [lindex $s_info 2]]
	    if { $id ne "" } {
		upvar #0 $id SERVICE
		set SERVICE(pid) $s_pids
		set SERVICE(state) RUNNING
	    }
	}
    }
}


# ::procwatch::start -- Start services
#
#	This procedure will start a number of services.  Do not start
#	them if they are alread on.  When several services are
#	started, the -pause option of the context will be honoured and
#	services will be started with pauses in between them.
#
# Arguments:
#	cx	Identifier of context
#	svcs	List of services to start (empty means all)
#	force	Force restarting of processes?
#
# Results:
#	Return the list of identifiers of the processes that were
#	effectively started.
#
# Side Effects:
#	None.
proc ::procwatch::start { cx {svcs {}} {force off}} {
    variable PW
    variable log

    # Reject if wrong context
    if { ![isa $cx context] } {
	${log}::warn "$cx is not a known context"
	return -code error "$cx is not a known context"
    }
    upvar #0 $cx CONTEXT

    # Find all services of the context if we did not specify any.
    if { [llength $svcs] == 0 } {
	set svcs [get $cx services]
    }

    # Now start services.
    set started ""
    set when 0
    foreach s $svcs {
	upvar #0 $s SERVICE

	if { [llength $SERVICE(pid)] > 0 && $force } {
	    ${log}::debug "Killing processes of service $SERVICE(id)"
	    foreach pid $SERVICE(pid) {
		::process::kill $pid
	    }
	    set SERVICE(pid) [list]
	    set SERVICE(state) DEAD
	}

	if { [llength $SERVICE(pid)] == 0 || $force } {
	    if { $SERVICE(state) ne "DISABLED" } {
		${log}::info "Scheduling service $s to start in $when ms."
		set SERVICE(state) STARTING
		set SERVICE(starting) [after $when ::procwatch::__exec $s]
		incr when [expr int($CONTEXT(-pause) * 1000)]
	    }
	}
    }
}


# ::procwatch::__projection -- List projection
#
#	This procedure returns the n'th index of a list.
#
# Arguments:
#	n	index
#	l	List to extract from
#
# Results:
#	Return the n'th index of a list
#
# Side Effects:
#	None.
proc ::procwatch::__projection { n l } {
    ::lindex $l $n
}


# ::procwatch::__intersect3 -- List intersection
#
#	Perform list intersection.
#
# Arguments:
#	list1	First list
#	list2	Second list
#
# Results:
#	Returns a list containing three lists.  The first list is
#	everything in the first list that wasn't in the second, the
#	second list contains the intersection of the two lists, the
#	third list contains everything in the second list that wasn't
#	in the first.
#
# Side Effects:
#	None.
#
# Copyright 1992-1999 Karl Lehenbauer and Mark Diekhans.
#
# Permission to use, copy, modify, and distribute this software and its
# documentation for any purpose and without fee is hereby granted, provided
# that the above copyright notice appear in all copies.  Karl Lehenbauer and
# Mark Diekhans make no representations about the suitability of this
# software for any purpose.  It is provided "as is" without express or
# implied warranty.
proc ::procwatch::__intersect3 {list1 list2} {
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


# ::procwatch::__watch -- Watch services
#
#	This procedure will arrange to regularly watch a number of
#	(started) services, restarting them if necessary.
#
# Arguments:
#	cx	Identifier of context
#	svcs	List of services to start (empty means all)
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::procwatch::__watch { cx { svcs "" } } {
    variable PW
    variable log

    # Reject if wrong context
    if { ![isa $cx context] } {
	${log}::warn "$cx is not a known context"
	return -code error "$cx is not a known context"
    }
    upvar #0 $cx CONTEXT

    # Find all services of the context if we did not specify any.
    if { [llength $svcs] == 0 } {
	set svc_watch [get $cx services]
    } else {
	set svc_watch $svcs
    }

    # Gather all pids of the services that we should watch
    set svc_pids [list]
    foreach s $svc_watch {
	upvar #0 $s SERVICE
	if { [llength $SERVICE(pid)] > 0 } {
	    lappend svc_pids [lindex $SERVICE(pid) 0]
	}
    }
    ${log}::debug "pids for our services should be: $svc_pids"
    
    # Gather information for all currently processes running on this
    # machine.  From this list, isolate services that should be
    # running but are not and restart services which virtual memory
    # usage has become greater than the allowed limit.
    set restart [list]
    if { [catch "::process::full_list" f_pids] == 0 } {
	# First restart processes that have disappeared and crashed
	set pids [::struct::list map $f_pids "__projection 0"]
	foreach {others running died} [__intersect3 $pids $svc_pids] {}

	# Parse through the list of processes that still do not exist
	# and find their corresponding service if any.  Restart all
	# those that are enabled and not currently already starting.
	if { [llength $died] > 0 } {
	    foreach d $died {
		set sid [find:bypid $d]
		if { $sid ne "" } {
		    upvar #0 $sid SERVICE
		    # Filter away services that we know are already
		    # starting or are disabled.
		    if { $SERVICE(state) ne "DISABLED" \
			     && $SERVICE(state) ne "STARTING" } {
			lappend restart $sid
		    }
		}
	    }

	    ${log}::notice "Some services have died, restarting: $restart"
	}

	# Now, restart processes that have grown too big.
	set toobig [list]
	foreach p_info $f_pids {
	    foreach {pid usr prg ppid msize} $p_info {}
	    set sid [find:bypid $pid]
	    if { $sid ne "" } {
		upvar #0 $sid SERVICE
		if { $SERVICE(state) eq "RUNNING" \
			 && $SERVICE(-maxsize) > 0 && $SERVICE(-maxsize) ne "" \
			 && $msize > $SERVICE(-maxsize) } {
		    ${log}::notice "In service $sid, $pid is $msize kB, i.e.\
                                    larger than the allowed $SERVICE(-maxsize)"
		    lappend toobig $sid
		}
	    }
	}
	if { [llength $toobig] > 0 } {
	    ${log}::notice "Some services are too big, restarting: $toobig"
	}

	# Do the restarting of all these in one pass only.
	set restart [lsort -unique [concat $restart $toobig]]
	if { [llength $restart] > 0 } {
	    start $cx $restart 1
	}
    } else {
	${log}::warn "Cannot get process list for now, sleeping again"
    }

    # Start the services that were still not started if we have autostart on
    if { [string is true $CONTEXT(-autostart)] } {
	foreach {tocheck unused1 unused2} [__intersect3 $svc_watch $restart] {}
	${log}::notice "Automatically checking [llength $tocheck] service(s)"
	if { [llength $tocheck] > 0 } {
	    start $cx $tocheck
	}
    }

    # Reschedule next check
    set CONTEXT(schedule) [after [expr $CONTEXT(-watch) * 1000] \
			       "::procwatch::__watch $cx \"$svcs\""]
}


# ::procwatch::kill -- Kill processes
#
#	Force killing of some or all of the current processes
#	associated to the services.  This procedure does not restart
#	these processes, however, if the processes are being watched,
#	they will automatically be restarted at the next contextual
#	check.
#
# Arguments:
#	svcs	List of service identifiers to kill or contexts, in which
#               case all the services of these context will be killed.
#
# Results:
#	Return the identifiers of the processes that were killed.
#
# Side Effects:
#	None.
proc ::procwatch::kill { { svcs {}} } {
    variable PW
    variable log

    set tokill [list]
    if { [llength $svcs] == 0 } {
	set tokill [info vars ::procwatch::service_*]
    } else {
	foreach id $svcs {
	    if { [isa $id context] } {
		foreach sid [get $id services] {
		    lappend tokill $sid
		}
	    } elseif { [isa $id service] } {
		lappend tokill $id
	    } else {
		${log}::warn "$id is neither a process context or a process"
	    }
	}
    }
    
    set killed [list]
    foreach s $tokill {
	upvar #0 $s SERVICE
	switch $SERVICE(state) {
	    STARTING {
		after cancel $SERVICE(starting)
		set SERVICE(pid) [list]
		set SERVICE(state) DEAD
	    }
	    RUNNING {
		foreach pid $SERVICE(pid) {
		    if { [::process::kill $pid] == $pid } {
			lappend killed $pid
		    } else {
			${log}::warn "Failed killing process $pid in service $s"
		    }
		}
		set SERVICE(pid) [list]
		set SERVICE(state) DEAD

		upvar #0 $SERVICE(context) CONTEXT
		if { $CONTEXT(rundir) ne "" } {
		    runfile:write $CONTEXT(id)
		}
	    }
	}
    }

    return $killed
}


# ::procwatch::__logout -- Callback log watches
#
#	This procedure is called wheneven a line has been output to
#	the log file of a watched service.  It relays the callbacks
#	that are associated to the watch.
#
# Arguments:
#	sid	Identifier of service
#	wid	logwatch identifier (unused)
#	line	log line
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::procwatch::__logout { sid wid line } {
    variable PW
    variable log

    puts "incoming line $line"
    upvar #0 $sid SERVICE
    foreach wid [get $sid logwatches] {
	upvar #0 $wid LOGWATCH
	${log}::debug "Forwarding '$line' to $wid"
	if { [catch {eval $LOGWATCH(command) $wid \$line} err] } {
	    ${log}::warn "Error when calling watch callback\
                          $LOGWATCH(command): $err"
	}
    }
}


# ::procwatch::log:watch -- Start watching a log
#
#	This command register a command to be called whenever output
#	is sent to the log file associated to a service.  The callback
#	will never be called back if the context has not been setup to
#	accomodate logs.
#
# Arguments:
#	sid	Identifier of a service
#	cmd	Command to call back, the identifier of the watch and
#               the log line will be appended to the command on log output.
#
# Results:
#	Return an identifier that can be used to unregister the watch,
#	empty string on error (no logfile, thus no log watch
#	possible).
#
# Side Effects:
#	None.
proc ::procwatch::log:watch { sid cmd } {
    variable PW
    variable log

    # Reject if wrong context
    if { ! [isa $sid service srid] } {
	${log}::warn "$sid is not a known service"
	return -code error "$sid is not a known service"
    }
    upvar #0 $sid SERVICE

    if { $SERVICE(wid) eq "" && $SERVICE(logfile) ne "" } {
	set SERVICE(wid) [::logwatch::new $SERVICE(logfile) \
			      [list ::procwatch::__logout $sid]]
	${log}::debug "Created log watch context for $SERVICE(logfile):\
                       $SERVICE(wid)"
    }

    if { $SERVICE(wid) ne "" } {
	isa $SERVICE(context) context cxid
	set varname \
	    [namespace current]::logwatch_${cxid}_${srid}_[incr PW(widgene)]
	upvar #0 $varname LOGWATCH
	set LOGWATCH(id) $varname
	set LOGWATCH(service) $sid
	set LOGWATCH(command) $cmd
	${log}::debug "Created new log watch on $sid, forwarding\
                       $SERVICE(logfile) to '$cmd'"
	return $LOGWATCH(id)
    }

    return ""
}


# ::procwatch::log:unwatch -- Stop one or several log watch
#
#	Remove one or several log watches.
#
# Arguments:
#	ids	List of identifiers, either log watches, service or context 
#               identifiers, in which case all log watches are stopped. An
#               empty list means all the logwatches of all contexts
#
# Results:
#	Return the list of log watches that were removed
#
# Side Effects:
#	None.
proc ::procwatch::log:unwatch { {ids {}}} {
    variable PW
    variable log

    # Gather in "unwatch" the list of logwatch identifiers that we
    # should stop watching.  This means recursing through the
    # contexts, and services if necessary.
    set unwatch [list]
    if { [llength $ids] == 0 } {
	set unwatch [info vars ::procwatch::logwatch_*]
    } else {
	foreach id $ids {
	    if { [isa $id context] } {
		foreach sid [get $id services] {
		    upvar #0 $sid SERVICE
		    foreach wid [get $sid logwatches] {
			lappend unwatch $wid
		    }
		}
	    } elseif { [isa $id service] } {
		foreach wid [get $id logwatches] {
		    lappend unwatch $wid
		}
	    } elseif { [isa $id logwatch] } {
		lappend unwatch $id
	    } else {
		${log}::warn "$id is neither a process context, a process or\
                              a log watch"
	    }
	}
    }
    
    # Now remove the logwatch, gathering in "svcs" the list of
    # services that hosted them.
    set svcs [list]
    foreach id $unwatch {
	upvar #0 $id LOGWATCH
	lappend svcs $LOGWATCH(service)
	unset LOGWATCH
    }

    # Stop watching the logfile for all services that do not have any
    # logwatch associated to them anymore, this is to avoid
    # unnecessary resource hog (shouldn't be, but I like to clean up).
    foreach sid [lsort -unique $svcs] {
	upvar #0 $sid SERVICE
	set watches [get $sid logwatches]
	if { [llength $watches] == 0 } {
	    ::logwatch::delete $SERVICE(wid)
	    set SERVICE(wid) ""
	}
    }

    return $unwatch
}


# ::procwatch::remove -- Remove services and contexts
#
#	Remove one or more services or contexts, killing their
#	associated processes if necessary.  Return the list of
#	processes that were killed.
#
# Arguments:
#	svcs	List of service identifiers to kill or contexts, in which
#               case all the services of these context will be killed.
#	force	Force killing of processes
#
# Results:
#	Return the identifiers of the processes that were killed.
#
# Side Effects:
#	None.
proc ::procwatch::remove { {svcs {}} {force 1}} {
    variable PW
    variable log

    # Arrange to gather the contexts and services that should be
    # removed in two different lists.
    set svc_toremove [list]
    set cx_toremove [list]
    if { [llength $svcs] == 0 } {
	set svc_toremove [info vars ::procwatch::service_*]
	set cx_toremove [info vars ::procwatch::context_*]
    } else {
	foreach id $svcs {
	    if { [isa $id context] } {
		foreach sid [get $id services] {
		    lappend svc_toremove $sid
		}
		lappend cx_toremove $id
	    } elseif { [isa $id service] } {
		lappend svc_toremove $id
	    } else {
		${log}::warn "$id is neither a process context or a process"
	    }
	}
    }
    
    # Kill services and remove their state
    ${log}::debug "Suppressing services $svc_toremove..."
    set killed [list]
    foreach s $svc_toremove {
	if { [string is true $force] } {
	    set killed [concat $killed [kill $s]]
	}
	upvar #0 $s SERVICE
	# Stop watching the log output
	if { $SERVICE(wid) ne "" } {
	    ::logwatch::delete $SERVICE(wid)
	}
	# Remove all logwatches on the log output, if any
	foreach v [get $s logwatches] {
	    unset $v
	}
	# Finally remove the service object
	unset SERVICE
    }

    # Remove all contexts and remove their state
    ${log}::debug "Removing contexts $cx_toremove..."
    foreach c $cx_toremove {
	upvar #0 $c CONTEXT
	if { $CONTEXT(schedule) ne "" } {
	    after cancel $CONTEXT(schedule)
	}
	unset CONTEXT
    }

    return $killed
}


# ::procwatch::get -- Get object properties
#
#	This procedure returns some semi-internal objects properties
#	to other modules.  The properties that are recognised are
#	dependent on the identifier of the object passed as a
#	parameter.  Any property led by a dash is a configuration
#	option.
#
# Arguments:
#	id	Identifier of object (context, service or logwatch).
#	type	Property to get
#
# Results:
#	The value of the property or an empty string
#
# Side Effects:
#	None.
proc ::procwatch::get { id type } {
    variable PW
    variable log

    if { ![isa $id context] && ![isa $id service] \
	     && ![isa $id logwatch] } {
	${log}::warn "$id is neither a process context, a service or a logwatch"
	return -code error \
	    "$id is neither a process context, a service or a logwatch"
    }

    if { [isa $id context] } {
	upvar #0 $id CONTEXT
	switch -glob -- $type {
	    "id" -
	    "logdir" -
	    "rundir" {
		return $CONTEXT($type)
	    }
	    "services" {
		set uscore [string last _ $id]
		set cxid [string range $id [expr $uscore +1] end]
		return [info vars "::procwatch::service_${cxid}_*"]
	    }
	    "-*" {
		return [config $id $type]
	    }
	}
    }

    if { [isa $id service] } {
	upvar #0 $id SERVICE
	switch -glob -- $type {
	    "id" -
	    "context" -
	    "exe" -
	    "cmd" -
	    "cmddir" -
	    "logfile" -
	    "state" -
	    "pid" {
		return $SERVICE($type)
	    }
	    "logwatches" {
		set uscore [string last _ $id]
		set srid [string range $id [expr $uscore +1] end]
		set uscore [string last _ $SERVICE(context)]
		set cxid [string range $SERVICE(context) \
				  [expr $uscore +1] end]
		return [info vars "::procwatch::logwatch_${cxid}_${srid}_*"]
	    }
	    "-*" {
		return [config $id $type]
	    }
	}
    }
    
    if { [isa $id logwatch] } {
	upvar #0 $id LOGWATCH
	switch -glob -- $type {
	    "id" -
	    "service" -
	    "command" {
		return $LOGWATCH($type)
	    }
	}
    }

    return ""
}


# ::procwatch::config -- Get/Set configuration options
#
#	This procedure will set or get the options of a service
#	context or a service.
#
# Arguments:
#	id	Identifier of the service or the context
#	args	list of options
#
# Results:
#	Return all options, the option requested or set the options
#
# Side Effects:
#	None.
proc ::procwatch::config { id args } {
    variable PW
    variable log

    if { [info vars $id] eq "" } {
	${log}::warn "$id is neither a process context or a process" 
	return -code error "$id is neither a process context or a process" 
    }

    if { [isa $id context] } {
	upvar #0 $id CONTEXT
	set result [eval ::uobj::config CONTEXT "-*" $args]
	if { $result eq "" } {
	    __initdirs $id
	    __uniqueness $id
	}
	if { $CONTEXT(schedule) eq "" } {
	    set CONTEXT(schedule) [after idle ::procwatch::__watch $id]
	}
	return $result
    } else {
	upvar #0 $id SERVICE
	set result [eval ::uobj::config SERVICE "-*" $args]
	if { $result eq "" } {
	    __resolv $id
	}
	return $result
    }
}


# ::procwatch::add -- Create a new service
#
#	This procedure creates a new service and associates it to the
#	controller which identifier is passed as a first argument.
#	The service will be continuously watched and restarted as soon
#	as it dies.
#
# Arguments:
#	cx	Idendifier of the controller, as returned by ::procwatch::new
#	args	A list of dash led options and their values
#
# Results:
#	Return the identifier of the service.  This identifier will be
#	used in many other procedures.
#
# Side Effects:
#	None.
proc ::procwatch::add { cx args } {
    variable PW
    variable log

    if { ![isa $cx context] } {
	${log}::warn "$cx is not a known context"
	return -code error "$cx is not a known context"
    }

    upvar #0 $cx CONTEXT
    set uscore [string last _ $cx]
    set cxid [string range $cx [expr $uscore +1] end]

    set varname [namespace current]::service_${cxid}_[incr PW(sidgene)]
    upvar #0 $varname SERVICE

    set SERVICE(id) $varname
    set SERVICE(context) $cx
    set SERVICE(exe) ""
    set SERVICE(cmd) ""
    set SERVICE(cmddir) ""
    set SERVICE(wid) ""
    set SERVICE(state) DEAD
    ::uobj::inherit PW SERVICE $PW(srv_inheritance)

    eval config $SERVICE(id) $args

    if { [string is true CONTEXT(autocapture)] } {
	__recapture $cx
    }

    return $SERVICE(id)
}


# ::procwatch::read -- Read service descriptions
#
#	This procedure will read a service description file into a
#	service context, thus creating new services.  The services
#	will be started or not, depending on the -autostart option of
#	the context.
#
# Arguments:
#	cx	Identifier of context
#	fname	Name of file
#
# Results:
#	Return the list of services that were created.
#
# Side Effects:
#	None.
proc ::procwatch::read { cx fname } {
    variable PW
    variable log

    if { ![isa $cx context] } {
	${log}::warn "$cx is not a known context"
	return -code error "$cx is not a known context"
    }

    upvar #0 $cx CONTEXT
    set CONTEXT(autocapture) off
    
    set services [list]

    if { [catch "open $fname r" sf] != 0 } {
	${log}::error "Unable to open $fname"
    } else {
	while { ! [eof $sf] } {
	    set line [string trim [gets $sf]]
	    if { $line ne "" } {
		set firstchar [string index $line 0]
		# Skip all lines that are commented.
		if { [string first $firstchar $PW(comments)] < 0 } {
		    set type [lindex $line 0]
		    set script [lindex $line 1]
		    set params [lrange $line 2 end]
		    set idx [string first "-- " $params]
		    if { $idx < 0 } {
			lappend services [add $cx -main $script -type $type \
					      -args $params]
		    } else {
			set vmparams [string range $params 0 [expr $idx - 1]]
			set params [string range $params [expr $idx + 3] end]
			lappend services [add $cx -main $script -type $type \
					      -args $params -vmargs $vmparams]
		    }
		}
	    }
	}
	close $sf
    }
    
    # Recapture running processes.
    __recapture $cx
    set CONTEXT(autocapture) on

    return $services
}


# ::procwatch::new -- Create new service controller
#
#	This procedure creates a new service controller context.  The
#	context will hold information about resources such as log
#	directories or run-time controlling directories.  Services
#	will have to be associated to a context to be created.
#
# Arguments:
#	args	A list of dash led options and their values
#
# Results:
#	Return the identifier of the context.  This identifier will be
#	used in many other procedures.
#
# Side Effects:
#	None.
proc ::procwatch::new { args } {
    variable PW
    variable log

    set varname [namespace current]::context_[incr PW(cidgene)]
    upvar #0 $varname CONTEXT

    set CONTEXT(id) $varname
    set CONTEXT(rundir) ""
    set CONTEXT(schedule) ""
    set CONTEXT(autocapture) on
    ::uobj::inherit PW CONTEXT $PW(cx_inheritance)

    eval config $CONTEXT(id) $args

    return $CONTEXT(id)
}

package provide procwatch 1.0
