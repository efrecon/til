# autoconnect.tcl --
#
#	This module is built on top of the permanent client library
#	and provides with two essential functions: sending and getting
#	information, i.e. asynchronous and synchronous communication
#	with the remote server.
#
# Copyright (c) 2004-2006 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

package require Tcl 8.2
package require logger

package require permclient

namespace eval ::autoconnect {
    # Initialise global state
    variable AC
    if {![info exists AC]} {
	array set AC {
	    loglevel   warn
	    clientseps "|:/ \t"
	    idgene     0
	    syncs      ""
	    fwdargs    "-poll -block -proxy"
	    dft_timeout 5000
	    dft_mark    "__T_I_M_E_O_U_T__"
	}
	variable log [::logger::init [string trimleft [namespace current] ::]]
	${log}::setlevel $AC(loglevel)
    }

    namespace export send get loglevel
}


# ::autoconnect::loglevel -- Set/Get current log level.
#
#	Set and/or get the current log level for this library.
#
# Arguments:
#	loglvl	New loglevel
#
# Results:
#	Return the current log level
#
# Side Effects:
#	None.
proc ::autoconnect::loglevel { { loglvl "" } } {
    variable AC
    variable log

    if { $loglvl != "" } {
	if { [catch "${log}::setlevel $loglvl"] == 0 } {
	    set AC(loglevel) $loglvl
	}
    }

    return $AC(loglevel)
}


# ::autoconnect::__incoming -- Internal callback for server comm. arrival
#
#	This procedure receives data from a remote server.  It
#	mediates back this information to the caller when a ::get call
#	is pending.
#
# Arguments:
#	id	Identifier of ::permclient:: connection to server.
#	line	Line coming from server.
#
# Results:
#	None
#
# Side Effects:
#	None.
proc ::autoconnect::__incoming { id line } {
    variable AC
    variable log

    # Guess where information is coming from.
    foreach {sock host port} [::permclient::info $id] {}
    ${log}::debug "Received from ${host}:${port}: $line"

    # Mediate back to the caller if requested.
    foreach {h p cmd s} $AC(syncs) {
	if { $h == $host && $p == $port } {
	    switch -- $cmd {
		"SET" {
		    set $s $line
		}
		"CB" {
		    foreach {cb ptn} $s {}
		    if { [string match $ptn $line] } {
			if { [catch {eval $cb $host $port \$line} err] } {
			    ${log}::warn \
				"Error when calling back '$cb' on $line: $err"
			}
		    }
		}
	    }
	}
    }
}


# ::autoconnect::receiver -- Setup a command receiver
#
#	Arrange so that a command will be called back every time a
#	line matching the pattern passed as an argument is received
#	from a remote client, as long as the connection exists.  Opens
#	the connection if necessary.
#
# Arguments:
#	dest	Remote server in the form of a host and a port,
#	        separated by one of :, |, / space or tab.
#	cb	Callback
#	ptn	Pattern for command
#	retry	How often to retry opening the connection
#
# Results:
#	None
#
# Side Effects:
#	None.
proc ::autoconnect::receiver { dest cb ptn args } {
    variable AC
    variable log

    # Split host and port of destination and push information to the
    # synchronisation queue.
    set dst [split $dest $AC(clientseps)]
    set host [lindex $dst 0]
    set port [lindex $dst 1]
    lappend AC(syncs) "$host" $port CB [list $cb $ptn]

    eval ::autoconnect::send $dest {""} $args
}


# ::autoconnect::__disconnect -- Disconnect a client
#
#	Disconnects an existing client.
#
# Arguments:
#	id	Identifier of permanent client.
#
# Results:
#	None
#
# Side Effects:
#	None.
proc ::autoconnect::__disconnect { id } {
    variable AC
    variable log

    foreach {sock host port} [::permclient::info $id] {}
    ${log}::info "Auto-disconnecting from ${host}:${port}"
    ::permclient::delete $id
}


proc ::autoconnect::__connected { id sock } {
    variable AC
    variable log

    ${log}::debug "Connected to client $id, flushing sending queue"
    set varname "::autoconnect::__cmdqueue_${id}"
    if { [info exists $varname] } {
	upvar \#0 $varname Queue

	foreach cmd $Queue {
	    ::permclient::write $id $cmd
	}
	unset Queue
    }
}


# ::autoconnect::send -- Asynchronously send to remote server
#
#	Send a command line to a remote server, automatically
#	connecting to this server if necessary.
#
# Arguments:
#	whomto  Remote server in the form of a host and a port,
#	        separated by one of :, |, / space or tab.
#	cmd	Command to send to server
#	tmout	Timeout when waiting for the connection to be established.
#
# Results:
#	Return 1 on success, 0 on error.
#
# Side Effects:
#	None.
proc ::autoconnect::send { whomto cmd args } {
    variable AC
    variable log

    # Isolate host and port number from the remote destination description
    set dst [split $whomto $AC(clientseps)]
    set host [lindex $dst 0]
    set port [lindex $dst 1]
    if { $host == "" || $port == "" } {
	${log}::warn "'$whomto' is an invalid destination!"
	return 0
    }

    # Find remote server in list of known servers and establish
    # connection if necessary.
    set asynchronous 0
    set id [::permclient::find $host $port]
    if { $id == "" } {
	set conncmd [list ::permclient::new $host $port \
			 ::autoconnect::__incoming \
			 -open ::autoconnect::__connected]
	foreach { arg val} $args {
	    if { [lsearch $AC(fwdargs) $arg] >= 0 } {
		lappend conncmd $arg $val
	    } elseif { $arg != "-autooff" } {
		${log}::warn "$arg is an unrecognised argument!"
	    }
	}
	set id [eval $conncmd]
	${log}::info "Connected to ${host}:${port}"
	set asynchronous 1
    }

    # Push into queue if connection to server is in progress
    foreach {sck hst prt} [::permclient::info $id] break
    if { $sck eq "" } {
	set asynchronous 1
    }

    # Auto disconnection
    set off -1
    foreach { arg val} $args {
	switch -- $arg {
	    "-autooff" { set off $val }
	}
    }
    if { $off >= 0 } {
	set varname ::autoconnect::autooff_${id}
	upvar \#0 $varname offid 
	if { [info exists $varname] } {
	    ${log}::debug \
		"Postponing current auto disconnection to $off secs."
	    after cancel $offid
	} else {
	    ${log}::debug \
		"Installing auto disconnection in $off secs."
	}
	set offid [after [expr {$off * 1000}] ::autoconnect::__disconnect $id]
    }

    # Send the command to the remote server
    if { $cmd != "" } {
	if { $asynchronous } {
	    ${log}::debug "Pushing command for sending on connect"
	    set varname "::autoconnect::__cmdqueue_${id}"
	    upvar $varname Queue
	    lappend Queue $cmd
	    set res 1
	} else {
	    set res [::permclient::write $id $cmd]
	    if { ! $res } {
		${log}::warn "Could not send '$cmd' to ${host}:${port}"
	    }
	}
    } else {
	set res 1
    }

    return $res
}


# ::autoconnect::__unblock -- Unblock the waiting for an answer
#
#	This procedure is meant to be posted as an after call and will
#	see to unblock the waiting for an answer from a remote server.
#
# Arguments:
#	varname Name of variable used for queue synchronisation
#	tmout_m	Value to set to the variable.
#
# Results:
#	None
#
# Side Effects:
#	None.
proc ::autoconnect::__unblock { varname tmout_m } {
    variable AC
    variable log

    if { [info vars $varname] != "" } {
	# Tell the caller if the variable still, exists, which means
	# that we are still waiting for the answer from the server.
	${log}::warn "Timout reached when waiting for answer"
	upvar $varname sync
	set sync $tmout_m
    }
}



# ::autoconnect::get -- Synchronously send to remote server
#
#	Send a command line to a remote server, automatically
#	connecting to this server if necessary.  Wait for the answer
#	and return it.
#
# Arguments:
#	dest    Remote server in the form of a host and a port,
#	        separated by one of :, |, / space or tab.
#	cmd	Command to send to server
#       args
#
# Results:
#	Return the answer from the server, or an empty string.
#
# Side Effects:
#	None.
proc ::autoconnect::get { dest cmd args } {
    variable AC
    variable log

    # Generate a synchronise variable that the reception procedure
    # will use to mediate back the answer from the server.
    set id [incr AC(idgene)]
    set varname "::autoconnect::__sync_${id}"
    upvar \#0 $varname sync
    set sync ""

    # Split host and port of destination and push information to the
    # synchronisation queue.
    set dst [split $dest $AC(clientseps)]
    set host [lindex $dst 0]
    set port [lindex $dst 1]
    lappend AC(syncs) "$host" $port SET $varname

    # Send the command to the destination and wait for the answer or
    # for a timeout.
    set tmout_m $AC(dft_mark)
    set tmout $AC(dft_timeout)
    set sendargs [list]
    foreach { arg val } $args {
	switch -- $arg {
	    "-mark" { set tmout_m $val }
	    "-timeout" { set tmout $val }
	    default { lappend sendargs $arg $val }
	}
    }
    after $tmout ::autoconnect::__unblock $varname "$tmout_m"
    if { [eval ::autoconnect::send $dest \$cmd $sendargs] } {
	vwait $varname
    } else {
	set sync $tmout_m
    }

    # Fetch result from the variable and return it back after some
    # clean up.
    if { $sync == $tmout_m } {
	set res ""
    } else {
	set res $sync
    }
    unset sync
    set idx [lsearch $AC(syncs) $varname]
    set AC(syncs) [lreplace $AC(syncs) [expr $idx - 3] $idx]

    return $res
}



package provide autoconnect 0.2
