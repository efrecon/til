# autoconnect.tcl --
#
#	This module is built on top of the permanent client library
#	and provides with two essential functions: sending and getting
#	information, i.e. asynchronous and synchronous communication
#       with the remote server. In addition, it allows connecting to a command
#       server that is being wrapped through websockify.
#
# Copyright (c) 2004-2006 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

package require Tcl 8.4
package require logger

package require permclient
package require websocket

namespace eval ::autoconnect {
    # Initialise global state
    variable AC
    if {![info exists AC]} {
	array set AC {
	    loglevel        warn
	    clientseps      "|:/ \t"
	    idgene          0
	    syncs           {}
	    passthrough_tcp {-poll -block -proxy}
	    passthrough_ws  {-binary -blocksize -channel -headers -keepalive -method -myaddr -progress -protocol -query -queryblocksize -querychannel -queryprogress -strict -timeout -type}
	    passthrough_lib {-autooff -wrapper -unwrapper}
	    dft_timeout     5000
	    dft_mark        "__T_I_M_E_O_U_T__"
	}
	array set HINTS {}
	variable log [::logger::init [string trimleft [namespace current] :]]
	${log}::setlevel $AC(loglevel)
    }

    namespace export send get loglevel hints
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

# ::autoconnect::__dispatch -- Dispatch incoming line
#
#       Dispatch data that has been received from the host and port to the
#       receivers, whenever the incoming data matches the pattern that was
#       associated with the receiver.
#
# Arguments:
#	host	Remote host sending the information
#	port	Port at which we are connected at the remote host.
#	id	Identifier of the connection (permclient or websocket)
#	line	Incoming command/info
#
# Results:
#       None.
#
# Side Effects:
#       None.
proc ::autoconnect::__dispatch { host port id line } {
    variable AC
    variable log

    # Mediate back to the caller if requested.
    foreach {h p cmd s} $AC(syncs) {
	if { $h eq $host && $p == $port } {
	    switch -- $cmd {
		"SET" {
		    set $s $line
		}
		"CB" {
		    # Replace command by its wrapped version, if relevant
		    if { [info exists [namespace current]::unwrapper_${id}] } {
			upvar \#0 [namespace current]::unwrapper_$id unwrapper
			
			if { [llength $unwrapper] } {
			    if { [catch {eval [linsert $unwrapper end $line]} res] } {
				${log}::warn "Could not execute unwrapper on line: $res"
			    } else {
				set line $res
			    }
			}
		    }

		    foreach {cb ptn} $s break
		    if { [string match $ptn $line] } {
			if { [catch {eval [linsert $cb end $host $port $line]} err] } {
			    ${log}::warn \
				"Error when calling back '$cb' on $line: $err"
			}
		    }
		}
	    }
	}
    }
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

    __dispatch $host $port $id $line
}


# ::autoconnect::__ws_handler -- Handler for websockets.
#
#       Handles life-time websocket connection events, i.e. incoming data,
#       connections, etc. Incoming textual data goes to __dispatch, notification
#       of connections goes to __connected.
#
# Arguments:
#	host	Remote host sending the information
#	port	Port at which we are connected at the remote host.
#	id	Identifier of the websocket connection
#	type	Type of the event
#	line	Incoming command/info
#
# Results:
#       None.
#
# Side Effects:
#       None.
proc ::autoconnect::__ws_handler { host port id type line } {
    variable AC
    variable log

    switch $type {
	"connect" {
	    __connected "ws" $id ""
	}
	"text" {
	    __dispatch $host $port $id $line
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
    foreach {proto host port} [__split $dest] break
    if { $proto ne "" && $host ne "" && $port ne "" } {
	set already 0
	foreach { h p t l } $AC(syncs) {
	    if { $h eq $host && $p == $port && $l eq [list $cb $ptn] } {
		set already 1
		break
	    }
	}

	if { ! $already } {
	    lappend AC(syncs) $host $port CB [list $cb $ptn]
	    eval [linsert $args 0 ::autoconnect::send $dest ""]
	}
    }
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
proc ::autoconnect::__disconnect { id proto } {
    variable AC
    variable log

    set varname [namespace current]::autooff_${id}
    if { [info exists $varname] } {
	unset $varname
    }

    switch -glob -- [string tolower $proto] {
	"ws*" {
	    ${log}::info "Auto-disconnecting"
	    ::websocket::close $id
	}
	"tcp" {
	    foreach {sock host port} [::permclient::info $id] {}
	    ${log}::info "Auto-disconnecting from ${host}:${port}"
	    ::permclient::delete $id
	}
    }
}

# ::autoconnect::__connected -- Connection initiation
#
#       This flushes the outgoing pending command queue upon connection to the
#       remote server.
#
# Arguments:
#	proto	Protocol (ws* or tcp)
#	id	Identifier of the connection (depends on protocol)
#	sock	Socket to server (for permclient)
#
# Results:
#       None.
#
# Side Effects:
#       Send as many commands as there are in the pending queue
proc ::autoconnect::__connected { proto id sock } {
    variable AC
    variable log

    ${log}::debug "Connected to client $id, flushing sending queue"
    set varname [namespace current]::__cmdqueue_${id}
    if { [info exists $varname] } {
	upvar \#0 $varname Queue

	foreach cmd $Queue {
	    __write $id $proto $cmd
	}
	unset Queue
    }
}


# ::autoconnect::__passthrough -- Collect relevant options
#
#       Collect protocol (or library) relevant options that can be passed
#       further to the connection opening commands.
#
# Arguments:
#	opts_p	"Pointer" to variable that will contain relevant options/values
#	type	Type of the connection (ws, tcp or lib (for internal options))
#	mixed	List of mixed options (internal and protocol specific)
#
# Results:
#       None.
#
# Side Effects:
#       Selects only relevant options and values and copy them to the variable
#       pointed by opts_p
proc ::autoconnect::__passthrough { opts_p type mixed { warn 1 } } {
    variable AC
    variable log
    
    # Select allowed options depending on the type.
    upvar $opts_p opts
    if { [info exists AC(passthrough_$type) ] } {
	set allowed $AC(passthrough_$type)
    } else {
	set allowed [list]
    }

    foreach { opt val } $mixed {
	if { [lsearch $allowed $opt] >= 0 } {
	    lappend opts $opt $val
	} elseif { [lsearch $AC(passthrough_lib) $opt] < 0 && $warn } {
	    ${log}::warn "$opt is an unrecognised argument!"
	}
    }
}


# ::autoconnect::__write -- Write to server
#
#       Send a command (line) to a remote server. This performs wrapping of the
#       command, if necessary, and sends using the proper API calls, depending
#       on the protocol.
#
# Arguments:
#	id	Identifier of the connection (websocket or permclient)
#	proto	Which protocol (ws* or tcp)
#	cmd	Line-based command to send to remote end
#
# Results:
#       None.
#
# Side Effects:
#       Sends command to remote end!
proc ::autoconnect::__write { id proto cmd } {
    variable AC
    variable log
    
    # Replace command by its wrapped version, if relevant
    if { [info exists [namespace current]::wrapper_${id}] } {
	upvar \#0 [namespace current]::wrapper_$id wrapper
	
	if { [llength $wrapper] } {
	    ${log}::info "Wrapping command '$cmd' using '$wrapper'"
	    if { [catch {eval [linsert $wrapper end $cmd]} res] } {
		${log}::warn "Could not execute wrapper on command: $res"
	    } else {
		set cmd $res
		${log}::debug "Wrapped original command to '$cmd'"
	    }
	}
    }
    
    # Now send the command.
    switch -glob -- [string tolower $proto] {
	"ws*" {
	    ::websocket::send $id text $cmd
	}
	"tcp" {
	    ::permclient::write $id $cmd
	}
    }
}


# ::autoconnect::__split -- URL split
#
#       Performs URL split and return the protocol, host and port that are
#       contained in the URL. This is compatible with prior versions of the
#       library that were built only on top of permclient and used a wider range
#       of possible separators.
#
# Arguments:
#	whomto	Specification of the remote end (usually a URL)
#
# Results:
#       A triplet of the protocol, the hostname and the port.
#
# Side Effects:
#       None.
proc ::autoconnect::__split { whomto } {
    variable AC
    variable log

    # Try understanding the destination as a URL, regular expression below is
    # taken from the http implementation.
    set URLmatcher {(?x)		# this is _expanded_ syntax
	^
	(?: (\w+) : ) ?			# <protocol scheme>
	(?: //
	    (?:
		(
		    [^@/\#?]+		# <userinfo part of authority>
		) @
	    )?
	    (				# <host part of authority>
		[^/:\#?]+ |		# host name or IPv4 address
		\[ [^/\#?]+ \]		# IPv6 address in square brackets
	    )
	    (?: : (\d+) )?		# <port part of authority>
	)?
	( [/\?] [^\#]*)?		# <path> (including query)
	(?: \# (.*) )?			# <fragment>
	$
    }
    if { ![regexp -- $URLmatcher $whomto -> proto user host port srvurl]} {
	# Isolate host and port number from the remote destination description,
	# old style
	set proto "tcp"
	foreach {host port} [split $whomto $AC(clientseps)] break
	if { $host eq "" || $port eq "" } {
	    ${log}::warn "'$whomto' is an invalid destination!"
	} else {
	    return [list $proto $host $port]
	}
    } else {
	return [list $proto $host $port]
    }
    
    return [list]
}


# ::autoconnect::disconnect -- Force disconnection
#
#       Force disconnection from the remote end.
#
# Arguments:
#	whomto	URL to remote (or old host:port specification)
#
# Results:
#       None.
#
# Side Effects:
#       None.
proc ::autoconnect::disconnect { whomto } {
    variable AC
    variable log
    
    foreach {proto host port} [__split $whomto] break
    if { $proto eq "" || $host eq "" || $port eq "" } {
	return 1
    }
    
    set id ""
    switch -glob -- [string tolower $proto] {
	"ws*" {
	    set id [::websocket::find $host $port]
	}
	"tcp" {
	    set id [::permclient::find $host $port]
	}
    }
    
    if { $id ne "" } {
	set varname [namespace current]::autooff_${id}
	if { [info exists $varname] } {
	    after cancel [set $varname]
	}
	__disconnect $id $proto
    }
}


# ::autoconnect::send -- Asynchronously send to remote server
#
#	Send a command line to a remote server, automatically
#       connecting to this server if necessary. This procedure is also able to
#       send commands to a remote websocket.
#
# Arguments:
#	whomto  Remote server in the form of a host and a port,
#	        separated by one of :, |, / space or tab, or a URL
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
    variable HINTS
    variable log

    foreach {proto host port} [__split $whomto] break
    if { $proto eq "" || $host eq "" || $port eq "" } {
	return 0
    }
    
    # Find remote server in list of known servers and establish
    # connection if necessary.
    set asynchronous 0
    set opts [list]
    switch -glob -- [string tolower $proto] {
	"ws*" {
	    if { [info exists HINTS(${host}:${port})] } {
		__passthrough opts "ws" $HINTS(${host}:${port})
	    }
	    __passthrough opts "ws" $args

	    set id [::websocket::find $host $port]
	    if { $id eq ""} {
		set conncmd [list ::websocket::open $whomto \
				    [list [namespace current]::__ws_handler $host $port]]
		set conncmd [concat $conncmd $opts]

		${log}::debug "Connecting websocket with: $conncmd"
		set id [eval $conncmd]
		${log}::info "Asynchronously connecting to ${host}:${port}/$proto"
		set asynchronous 1
	    } elseif { [::websocket::conninfo $id state] ne "CONNECTED" } {
		set asynchronous 1
	    }
	}
	"tcp" {
	    if { [info exists HINTS(${host}:${port})] } {
		__passthrough opts "tcp" $HINTS(${host}:${port})
	    }
	    __passthrough opts "tcp" $args

	    set id [::permclient::find $host $port]
	    if { $id eq "" } {
		set conncmd [list ::permclient::new $host $port \
				 [namespace current]::__incoming \
				 -open [list [namespace current]::__connected $proto]]
		set conncmd [concat $conncmd $opts]

		${log}::debug "Connecting permanent client with: $conncmd"
		set id [eval $conncmd]
		${log}::info "Asynchronously connecting to ${host}:${port}/$proto"
		set asynchronous 1
	    }
	
	    # Push into queue if connection to server is in progress
	    foreach {sck hst prt} [::permclient::info $id] break
	    if { $sck eq "" } {
		set asynchronous 1
	    }
	}
	default {
	    ${log}::warn "Unknown protocol $proto"
	    return 0
	}
    }

    # Extracting auto disconnection and wrappers information from options (and
    # hints)
    set opts {}
    if { [info exists HINTS(${host}:${port})] } {
	__passthrough opts "lib" $HINTS(${host}:${port}) 0
    }
    __passthrough opts "lib" $args 0
    set off -1
    set wrapper [list]
    set unwrapper [list]
    foreach { arg val} $opts {
	switch -- $arg {
	    "-autooff" { set off $val }
	    "-wrapper" { set wrapper $val }
	    "-unwrapper" { set unwrapper $val }
	}
    }

    # Register disconnection timer whenever relevant    
    if { $off >= 0 } {
	set varname [namespace current]::autooff_${id}
	upvar \#0 $varname offid 
	if { [info exists $varname] } {
	    ${log}::debug "Postponing current auto disconnection to $off secs."
	    after cancel $offid
	} else {
	    ${log}::debug "Installing auto disconnection in $off secs."
	}
	set offid [after [expr {$off * 1000}] \
		   [namespace current]::__disconnect $id [string tolower $proto]
    }

    # Always set the wrappers to something, even though it is an empty list    
    set [namespace current]::wrapper_${id} $wrapper
    set [namespace current]::unwrapper_${id} $unwrapper

    # Send the command to the remote server
    if { $cmd != "" } {
	if { $asynchronous } {
	    ${log}::debug "Pushing command $cmd for sending upon connection"
	    set varname "::autoconnect::__cmdqueue_${id}"
	    upvar $varname Queue
	    lappend Queue $cmd
	    set res 1
	} else {
	    set res [__write $id $proto $cmd]
	    if { ! $res } {
		${log}::warn "Could not send '$cmd' to ${host}:${port}"
	    }
	}
    } else {
	set res 1
    }

    return $res
}


# ::autoconnect::hints -- Sending hints
#
#       Provide and store hints for a remote host and port to which we will be
#       sending later on. 
#
# Arguments:
#	host	Name of remote host
#	port	Port at remote host
#       args	Dash-led options and arguments, passed to protocol-specific opening
#       	commands or interal to library 
#
# Results:
#       Return current hints.
#
# Side Effects:
#       None.
proc ::autoconnect::hints { host port args } {
    variable HINTS
    
    if { [llength $args] } {
	set HINTS(${host}:${port}) $args
    }
    if { [info exists HINTS(${host}:${port})] } {
	return $HINTS(${host}:${port})
    }
    return [list]
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
    if { [eval [list ::autoconnect::send $dest $cmd $sendargs]] } {
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


package provide autoconnect 0.3
