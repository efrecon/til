##################
## Program Name    --  log_duplicate.tcl
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##    Duplicate the log files from a remote starter into some local files.
##
##################
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

array set LD {
    finish         0
    permid         ""
    remotes        ""
    svcs           ""
}

source [file join [file dirname $argv0] argutil.tcl]
argutil::accesslib tcllib

# Now parse the options and put the result into the GE global state array
package require cmdline

set options {
    { verbose.arg "warn" "Verbosity Level" }
    { host.arg "localhost" "Host name of server to connect to" }
    { port.integer "3448" "Port number of server to connect to" }
    { services.arg "*" "List of patterns that remotes services should match to be shadowed" }
    { logdir.arg "%progdir%/log" "directory for logging of services output, empty to disable (sub of 'home')" }
    { rotate.double "168" "Number of hours before rotating, negative to switch off" }
    { keep.integer "4" "How many log files should we keep when rotating?" }
    { period.integer "60" "Period in seconds for process list info trigger" }
}

set inited [argutil::initargs LD $options]
if { [catch {cmdline::typedGetoptions argv $options} optlist] != 0 } {
    puts [cmdline::typedUsage $options "accepts the following options:"]
    exit
}
array set LD $optlist
foreach key $inited {
    argutil::makelist LD($key)
}

# Include modules that we depend on.  This is complicated to be able
# to address separately modules in the verbose specification.
argutil::accesslib til
argutil::loadmodules [list permclient diskutil outlog] $LD(verbose)

# Initialise local logging facility
package require logger
set LD(log) [::logger::init logdup]
$LD(log)::setlevel $LD(verbose)
argutil::fix_outlog

# Command Name     --  trigger_remote_list
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# This procedure trigger the generation of the remote list of processes
# under control.
#
# Arguments:
#    period	- Time for rescheduling.
proc trigger_remote_list { id { period -1 } } {
    global LD

    $LD(log)::info "Triggering remote list"
    set LD(old_remotes) $LD(remotes)
    set LD(remotes) ""
    ::permclient::write $id "LIST"

    if { $period >= 0 } {
	after $period trigger_remote_list $id $period
    }
}


# Command Name     --  open_cb
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# This command is called each time the connection to the client is opened.
# We trigger list information to know about the current remote programs.
#
# Arguments:
#    id 	- ID of permanent client.
#    sock	- Socket connection to client.
proc open_cb { id sock } {
    trigger_remote_list $id
}


# Command Name     --  incoming
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Command called back for each line coming from the remote starter process.
#
# Arguments:
#    id 	- ID of permanent client
#    line	- Incoming line.
proc incoming { id line } {
    global LD

    set spc [string first " " $line]
    if { $spc >= 0 } {
	# Isolate the command.
	set cmd [string toupper [string range $line 0 [expr $spc - 1]]]
	
	switch $cmd {
	    LIST {
		# The list of remote services arrives. Remove services
		# that were registered and have disappeared and
		# request information for each (new) one.
		set LD(remotes) ""
		set l [string range $line [expr $spc + 1] end]
		for { set i 0 } { $i < [llength $l] } { incr i 2 } {
		    lappend LD(remotes) [lindex $l $i]
		}
		# De-register old services.
		foreach sid $LD(old_remotes) {
		    set idx [lsearch $LD(remotes) $sid]
		    if { $idx < 0 } {
			set idx [lsearch $LD(svcs) $sid]
			if { $idx >= 0 } {
			    set varname "__Service_$sid"
			    upvar \#0 $varname Service

			    if { $Service(outid) != "" } {
				::outlog::close $Service(outid)
			    }
			    unset Service
			    set LD(svcs) [lreplace $LD(svcs) $idx $idx]
			}
		    }
		}
		::permclient::write $LD(permid) "INFO $LD(remotes)"
	    }
	    INFO {
		# One remote service information arrives.  Register
		# this service if it matches our list of authorised
		# local services and open a local log file for
		# writing.
		set sid [lindex $line 1]
		set pids [lindex $line 2]
		set remote_log [lindex $line 3]
		set prg [lrange $line 4 end]
		
		# Check whether the name of the remote program matches
		# our list of authorised patterns, only those that
		# match will be registered as local services.
		foreach ptn $LD(services) {
		    if { [string match $ptn $prg] } {
			# Create/access the global instance of that
			# service.
			set varname "__Service_$sid"
			upvar \#0 $varname Service

			# Initialise things if this is a new service.
			set idx [lsearch $LD(svcs) $sid]
			if { $idx < 0 } {
			    set Service(outid) ""
			    lappend LD(svcs) $sid
			}

			# (re)initialise with the values from the
			# remote service. The name of the log file
			# will be the same.
			set Service(sid) $sid
			set Service(pids) $pids
			set Service(remote_log) $remote_log
			if { $LD(logdir) != "" } {
			    set rawname [file tail $remote_log]
			    set Service(local_log) \
				[file join $LD(logdir) $rawname]
			} else {
			    set Service(local_log) ""
			}

			# Now, we we have a log directory, open the
			# local log file for writing.  We use the
			# outlog service to handle NFS and local
			# failures appropriately.
			if { $Service(outid) == "" \
				 && $Service(local_log) != "" } {
			    $LD(log)::notice \
				"Watching $sid into $Service(local_log)"
			    set Service(outid) \
				[::outlog::open $Service(local_log) \
				     $LD(rotate) $LD(keep)]
			}

			# Make sure that we start watching the remote
			# service.  starter.tcl gracefully accepts
			# several WATCH commands for the same id.
			if { $Service(outid) != "" } {
			    ::permclient::write $LD(permid) "WATCH $sid"
			}
		    }
		}
	    }
	    LOG {
		# Incoming log command.  If it is one of our
		# recognised services, forward the line to the local
		# log.
		set snd_spc [string first " " $line [expr $spc + 1]]
		if { $snd_spc >= 0 } {
		    set sid [string trim [string range $line $spc $snd_spc]]
		    set idx [lsearch $LD(svcs) $sid]
		    if { $idx >= 0 } {
			# This log line comes from one of our
			# registered and authorised services, forward
			# to the local log file.
			set varname "__Service_$sid"
			upvar \#0 $varname Service
			if { $Service(outid) != "" } {
			    ::outlog::puts $Service(outid) \
				[string range $line [expr $snd_spc + 1] end]
			}
		    }
		}
	    }
	}
    }
}

# Create log directory if necessary.  Disable logging facilities if we
# could not create it.
if { $LD(logdir) != "" } {
    set LD(logdir) [::diskutil::fname_resolv $LD(logdir)]
    if { ! [file exists $LD(logdir)] } {
	$LD(log)::notice "Creating logging directory $LD(logdir)"
	file mkdir $LD(logdir)
    } elseif { ! [file isdirectory $LD(logdir)] } {
	$LD(log)::error \
		"Logging directory $LD(logdir) is not a directory!"
	set LD(logdir) ""
    }
    if { $LD(logdir) != "" } {
	set LD(logdir) [::diskutil::absolute_path $LD(logdir)]
    }
}

# Arrange to be a permanent client of the remote server.
set LD(permid) [::permclient::new $LD(host) $LD(port) incoming -open open_cb]

# Trigger periodic check of the list of remote services, if requested
if { $LD(period) > 0 } {
    set period [expr $LD(period) * 1000]
    after $period trigger_remote_list $LD(permid) $period
}

vwait LD(finish)
