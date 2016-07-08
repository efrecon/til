# autoconnect.tcl --
#
#	This module is built on top of the permanent client library
#	and provides with two essential functions: sending and getting
#	information, i.e. asynchronous and synchronous communication
#       with the remote server. In addition, it allows connecting to a command
#       server that is being wrapped through websockify, thus implementing the
#       same kind of semantics over websockets instead.
#
#       Connections are identified by their canonicalized URL. The module is
#       *almost* backwards compatible with a prior version that was solely built
#       on top of the permclient library. In this prior version, a remote server
#       was identified by a hostname and port, separated using a range of
#       different "good" separators. This old syntax is automatically recognised
#       and replaced by tcp://hostname:port/ canonicalised URLs.
#
#       Note that callbacks delivered on command (line) reception have changed
#       as they take the URL and the line as arguments only in this new version.
#
# Copyright (c) 2004-2006 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.


package require Tcl 8.4
package require logger

package require uobj
package require permclient
package require websocket
package require base64

namespace eval ::autoconnect {
    # Initialise global state
    variable AC
    if {![info exists AC]} {
	array set AC {
	    -autooff        -1
	    -wrapper        {}
	    -unwrapper      {}
	    -framing        "text"
	    -cap            25
	    clientseps      "|:/ \t"
	    idgene          0
	    syncs           {}
	    passthrough_tcp {-poll -block -proxy}
	    passthrough_ws  {-binary -blocksize -channel -headers -keepalive -method -myaddr -progress -protocol -query -queryblocksize -querychannel -queryprogress -strict -timeout -type}
	    passthrough_lib {-autooff -wrapper -unwrapper -framing}
	    hints           {}
	    dft_timeout     5000
	    dft_mark        "__T_I_M_E_O_U_T__"
	}
	variable libdir [file dirname [file normalize [info script]]]
	::uobj::install_log autoconnect AC
	::uobj::install_defaults autoconnect AC
    }

    namespace export send get loglevel hints receiver
}


# TODO:
#
# Make this even more general, since most of the code for permclient and
# websocket is very similar. The procedures and commands to use could be given
# to autoconnect, perhaps through plugins?
#
# Implement http support so we can do ::autoconnect::get http://xxx and get back
# the result once done


# ::autoconnect::__dispatch -- Dispatch incoming line
#
#       Dispatch data that has been received from the remote URL to the
#       receivers, whenever the incoming data matches the pattern that was
#       associated with the receiver.
#
# Arguments:
#	cx	Identifier of the connection
#	id	Low-level identifier of the connection (permclient or websocket)
#	line	Incoming command/info
#
# Results:
#       None.
#
# Side Effects:
#       None.
proc ::autoconnect::__dispatch { cx id line } {
    variable AC
    variable log

    if { ![::uobj::isa $cx connection] } {
	return -code error "$cx unknown or wrong type"
    }
    upvar \#0 $cx CONN

    # Mediate back to the caller if requested.
    foreach {cmd s} $CONN(syncs) {
	switch -- $cmd {
	    "SET" {
		set $s $line
	    }
	    "CB" {
		# Replace command by its wrapped version, if relevant
		if { [info exists CONN(unwrapper)] } {
		    if { [llength $CONN(unwrapper)] } {
			${log}::info "Unwrapping incoming command using '$CONN(unwrapper)'"
			if { [catch {eval [linsert $CONN(unwrapper) end $line]} res] } {
			    ${log}::warn "Could not execute unwrapper on line: $res"
			} else {
			    set line $res
			}
		    }
		}

		foreach {cb ptn} $s break
		if { [string match $ptn $line] } {
		    if { [catch {eval [linsert $cb end $CONN(url) $line]} err] } {
			${log}::warn \
			    "Error when calling back '$cb' on $line: $err"
		    }
		}
	    }
	}
    }
}


# ::autoconnect::__ws_handler -- Handler for websockets.
#
#       Handles life-time websocket connection events, i.e. incoming data,
#       connections, etc. Incoming textual data goes to __dispatch, notification
#       of connections goes to __connected.
#
# Arguments:
#	cx	Identifier of the connection
#	sock	Low-level identifier of the websocket connection
#	type	Type of the event
#	line	Incoming command/info
#
# Results:
#       None.
#
# Side Effects:
#       None.
proc ::autoconnect::__ws_handler { cx sock type line} {
    variable AC
    variable log

    if { ![::uobj::isa $cx connection] } {
	return -code error "$cx unknown or wrong type"
    }
    upvar \#0 $cx CONN

    switch $type {
	"connect" {
	    __connected $cx $sock $sock;   # The identifier is the socket
	}
	"text" -
	"binary" {
	    __dispatch $cx $sock $line
	}
	"disconnect" {
	    __disconnected $cx $sock;      # The identifier is the socket
	    set CONN(id) "";               # Will force reconnection later
	}
    }
}


proc ::autoconnect::__disconnected { cx id } {
    variable AC
    variable log

    if { ![::uobj::isa $cx connection] } {
	return -code error "$cx unknown or wrong type"
    }
    upvar \#0 $cx CONN

    __liveness $cx "DISCONNECTED"
}


proc ::autoconnect::liveness { dest cb } {
    variable AC
    variable log

    set cx [__context $dest]
    if { $cx eq "" } {
	return ""
    }
    
    upvar \#0 $cx CONN
    lappend CONN(liveness) $cb
}


proc ::autoconnect::__liveness { cx state } {
    variable AC
    variable log

    if { ![::uobj::isa $cx connection] } {
	return -code error "$cx unknown or wrong type"
    }
    upvar \#0 $cx CONN
    
    foreach cb $CONN(liveness) {
	if { [catch {eval [linsert $cb end $CONN(url) $state]} err] } {
	    ${log}::warn "Error when delivering liveness callback: $err"
	}
    }
}


# ::autoconnect::receiver -- Setup a command receiver
#
#	Arrange so that a command will be called back every time a
#	line matching the pattern passed as an argument is received
#	from a remote URL, as long as the connection exists.  Opens
#	the connection if necessary.
#
# Arguments:
#	dest	Remote server in the form of a host and a port,
#	        separated by one of :, |, / space or tab, or URL.
#	cb	Callback
#	ptn	Pattern for command
#	args	Additional command-line arguments to the send
#
# Results:
#	1 on success, 0 on failure
#
# Side Effects:
#	None.
proc ::autoconnect::receiver { dest cb ptn args } {
    variable AC
    variable log

    set cx [__context $dest]
    if { $cx eq "" } {
	return 0
    }
    upvar \#0 $cx CONN

    set already 0
    foreach { t l } $CONN(syncs) {
	if { $l eq [list $cb $ptn] } {
	    set already 1
	    break
	}
    }

    if { ! $already } {
	# Arrange to receive callbacks and open connection
	lappend CONN(syncs) CB [list $cb $ptn]
	eval [linsert $args 0 [namespace current]::send $dest ""]
    }
    
    return 1
}


# ::autoconnect::__disconnect -- Disconnect a connection
#
#	Disconnects an existing connection
#
# Arguments:
#	id	Identifier of the connection.
#
# Results:
#	None
#
# Side Effects:
#	None.
proc ::autoconnect::__disconnect { cx } {
    variable AC
    variable log

    if { ![::uobj::isa $cx connection] } {
	return -code error "$cx unknown or wrong type"
    }
    upvar \#0 $cx CONN

    switch -glob -- $CONN(proto) {
	"ws*" {
	    ${log}::info "Auto-disconnecting from $CONN(url)"
	    ::websocket::close $CONN(id)
	    # Do not empty CONN(id) as we will do this in the disconnection
	    # callback.
	}
	"tcp" {
	    ${log}::info "Auto-disconnecting from $CONN(url)"
	    ::permclient::delete $CONN(id)
	    set CONN(id) "";               # Will force reconnection later
	}
    }
}

# ::autoconnect::__connected -- Connection initiation
#
#       This flushes the outgoing pending command queue upon connection to the
#       remote server.
#
# Arguments:
#	cx	Identifier of the connection
#	id	Low-level identifier of the connection (permclient/websocket)
#	sock	Socket to server
#
# Results:
#       None.
#
# Side Effects:
#       Send as many commands as there are in the pending queue
proc ::autoconnect::__connected { cx {id "" } {sock ""}} {
    variable AC
    variable log

    if { ![::uobj::isa $cx connection] } {
	return -code error "$cx unknown or wrong type"
    }
    upvar \#0 $cx CONN

    __liveness $cx "CONNECTED"

    ${log}::debug "Connected to $CONN(url), flushing sending queue"
    foreach cmd $CONN(queue) {
	__write $cx $cmd
    }
    set CONN(queue) [list]
    set CONN(sock) $sock
}


# ::autoconnect::__passthrough -- Collect relevant options
#
#       Collect protocol (or library) relevant options that can be passed
#       further to the connection respective opening commands in permclient or
#       websocket.
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
#       Send a command (line) to a remote connection. This performs wrapping of
#       the command, if necessary, and sends using the proper API calls,
#       depending on the protocol.
#
# Arguments:
#	cx	Identifier of the connection
#	cmd	Line-based command to send to remote end
#
# Results:
#       None.
#
# Side Effects:
#       Sends command to remote end!
proc ::autoconnect::__write { cx cmd } {
    variable AC
    variable log
    
    if { ![::uobj::isa $cx connection] } {
	return -code error "$cx unknown or wrong type"
    }
    upvar \#0 $cx CONN
    
    # Replace command by its wrapped version, if relevant
    if { [info exists CONN(wrapper)] } {
	if { [llength $CONN(wrapper)] } {
	    ${log}::info "Wrapping command '$cmd' using '$CONN(wrapper)'"
	    if { [catch {eval [linsert $CONN(wrapper) end $cmd]} res] } {
		${log}::warn "Could not execute wrapper on command: $res"
	    } else {
		set cmd $res
	    }
	}
    }
    
    # Now send the command.
    switch -glob -- $CONN(proto) {
	"ws*" {
	    ::websocket::send $CONN(id) $CONN(framing) $cmd
	}
	"tcp" {
	    ::permclient::write $CONN(id) $cmd
	}
    }
}


# ::autoconnect::__canonicalize -- URL extraction and (re)creation
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
#       A URL for the protocols that we support, or an empty string.
#
# Side Effects:
#       None.
proc ::autoconnect::__canonicalize { whomto {proto_p ""} {host_p ""} {port_p ""} { user_p "" } } {
    variable AC
    variable log

    # Access caller's variables
    if { $proto_p ne "" } { upvar $proto_p proto }
    if { $host_p ne "" } { upvar $host_p host }
    if { $port_p ne "" } { upvar $port_p port }
    if { $user_p ne "" } { upvar $user_p user }
    
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
	set proto "tcp"; set user ""
	foreach {host port} [split $whomto $AC(clientseps)] break
	if { $host eq "" || $port eq "" } {
	    set proto ""; set host ""; set port ""
	    ${log}::warn "'$whomto' is an invalid destination!"
	    return ""
	} else {
	    set host [string tolower $host]
	    if { ! [string is integer -strict $port] } {
		${log}::warn "$port is not an integer!"
		return ""
	    }
	    return ${proto}://${host}:${port}/
	}
    } else {
	set proto [string tolower $proto]
	set host [string tolower $host]
	if { $proto eq "tcp" } {
	    if { [string is integer -strict $port] } {
		# Always reconstruct the URL to get a consequent trailing slash
		return ${proto}://${host}:${port}/
	    } else {
		${log}::warn "$port is not an integer!"
	    }
	} elseif { [string match "ws*" $proto] } {
	    if { $port eq "" } {
		if { $proto eq "ws" } { set port 80 }
		if { $proto eq "wss" } { set port 443 }
	    }
	    if { [string is integer -strict $port] } {
		return $whomto
	    } else {
		${log}::warn "$port is not an integer!"
	    }
	} else {
	    ${log}::warn "$proto is not a recognised protocol"
	}
    }
    
    return "";  # Default for all errors
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
proc ::autoconnect::disconnect { whomfrom } {
    variable AC
    variable log
    
    set url [__canonicalize $whomfrom]
    if { $url ne "" } {
	set cx [::uobj::find [namespace current] connection [list url == $url]]
	if { $cx ne "" } {
	    upvar \#0 $cx CONN
	    
	    if { [info exists CONN(offtimer)] && $CONN(offtimer) ne "" } {
		after cancel $CONN(offtimer)
		set CONN(offtimer) ""
	    }
	    __disconnect $cx
	}
    }
}


proc ::autoconnect::__context { dest } {
    variable AC
    variable log

    set url [__canonicalize $dest proto host port auth]
    if { $url eq "" } {
	${log}::warn "$dest does not refer to a valid endpoint!"
	return ""
    }
    
    set cx [::uobj::find [namespace current] connection [list url == $url]]
    if { $cx eq "" } {
	set cx [::uobj::new [namespace current] connection]
	upvar \#0 $cx CONN
	set CONN(self) $cx
	set CONN(url) $url
	set CONN(proto) $proto
	set CONN(host) $host
	set CONN(port) $port
	set CONN(auth) $auth
	set CONN(queue) [list]
	set CONN(id) ""
	set CONN(framing) ""
	set CONN(sock) ""
	set CONN(syncs) [list]
	set CONN(liveness) [list]
    }
    
    return $cx
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
    variable log

    set cx [__context $whomto]
    if { $cx eq "" } {
	return 0
    }
    upvar \#0 $cx CONN
    
    # Extracting auto disconnection and wrappers information from options (and
    # hints)
    set opts [list]
    foreach {ptn arguments} $AC(hints) {
	if { [string match $ptn $CONN(url)] } {
	    __passthrough opts "lib" $arguments 0
	}
    }
    __passthrough opts "lib" $args 0
    set off $AC(-autooff)
    set CONN(wrapper) $AC(-wrapper)
    set CONN(unwrapper) $AC(-unwrapper)
    set CONN(framing) $AC(-framing)
    set CONN(cap) $AC(-cap)
    foreach { arg val} $opts {
	switch -- $arg {
	    "-autooff" { set off $val }
	    "-wrapper" { set CONN(wrapper) $val }
	    "-unwrapper" { set CONN(unwrapper) $val }
	    "-framing" { set CONN(framing) $val }
	    "-cap" { set CONN(cap) $val }
	}
    }

    # Find remote server in list of known servers and establish
    # connection if necessary.
    set asynchronous 0
    set opts [list]
    switch -glob -- [string tolower $CONN(proto)] {
	"ws*" {
	    foreach {ptn arguments} $AC(hints) {
		if { [string match $ptn $CONN(url)] } {
		    __passthrough opts "ws" $arguments
		}
	    }
	    __passthrough opts "ws" $args

	    if { $CONN(id) eq "" } {
		set conncmd [list ::websocket::open $CONN(url) \
				    [list [namespace current]::__ws_handler $cx]]
		if { $CONN(auth) ne "" } {
		    array set HDRS [list]
		    set nopts [list]
		    foreach {k v} $opts {
			if { [string match -he* $k] } {
			    array set HDRS $v
			} else {
			    lappend nopts $k $v
			}
		    }
		    set HDRS(Authorization) "Basic [::base64::encode $CONN(auth)]"
		    lappend nopts -headers [array get HDRS]
		    set conncmd [concat $conncmd $nopts]
		} else {
		    set conncmd [concat $conncmd $opts]
		}
		${log}::debug "Connecting websocket with: $conncmd"
		set CONN(id) [eval $conncmd]
		${log}::info "Asynchronously connecting to $CONN(url)"
		set asynchronous 1
	    }
	    
	    # Existing connection, but not yet connected
	    if { [::websocket::conninfo $CONN(id) state] ne "CONNECTED" } {
		set asynchronous 1
	    }
	}
	"tcp" {
	    foreach {ptn arguments} $AC(hints) {
		if { [string match $ptn $CONN(url)] } {
		    __passthrough opts "tcp" $arguments
		}
	    }
	    __passthrough opts "tcp" $args

	    if { $CONN(id) eq "" } {
		set conncmd [list ::permclient::new $CONN(host) $CONN(port) \
				 [list [namespace current]::__dispatch $cx] \
				 -open [list [namespace current]::__connected $cx] \
				 -down [list [namespace current]::__disconnected $cx]]
		set conncmd [concat $conncmd $opts]

		${log}::debug "Connecting permanent client with: $conncmd"
		set CONN(id) [eval $conncmd]
		${log}::info "Asynchronously connecting to $CONN(url)"
		set asynchronous 1
	    }
	    
	    # Push into queue if connection to server is in progress
	    foreach {sck hst prt} [::permclient::info $CONN(id)] break
	    if { $sck eq "" } {
		set asynchronous 1
	    }
	}
	default {
	    # Should not happen
	    ${log}::warn "Unknown protocol $proto"
	    return 0
	}
    }

    # Register disconnection timer whenever relevant    
    if { $off >= 0 } {
	if { [info exists CONN(offtimer)] && $CONN(offtimer) ne "" } {
	    ${log}::debug "Postponing current auto disconnection to $off secs."
	    after cancel $CONN(offtimer)
	} else {
	    ${log}::debug "Installing auto disconnection in $off secs."
	}
	set CONN(offtimer) \
	    [after [expr {$off*1000}] [list [namespace current]::__disconnect $cx]
    } else {
	set CONN(offtimer) ""
    }

    # Send the command to the remote server
    if { $cmd != "" } {
	if { $asynchronous } {
	    set res [__enqueue $cx $cmd]
	} else {
	    set res [__write $cx $cmd]
	    if { ! $res } {
		${log}::warn "Could not send '$cmd' to $CONN(url)!"
		set res [__enqueue $cx $cmd]
	    }
	}
    } else {
	set res 1
    }

    return $res
}


proc ::autoconnect::__enqueue { cx cmd } {
    variable AC
    variable log
    
    if { ![::uobj::isa $cx connection] } {
	return -code error "$cx unknown or wrong type"
    }
    upvar \#0 $cx CONN

    # When the queue cap is negative, append commands infinitely. When positive,
    # ensure that we have at most that many commands. When zero, no command will
    # be enqueued!
    if { $CONN(cap) < 0 } {
	${log}::debug "Pushing command $cmd for sending upon connection"
	lappend CONN(queue) $cmd
	return 1
    } elseif { $CONN(cap) > 0 } {
	if { [llength $CONN(queue)] >= $CONN(cap) } {
	    ${log}::warn "More than $CONN(cap) waiting in queue, loosing oldest one"
	    set CONN(queue) [lrange $CONN(queue) 1 end]
	}
	${log}::debug "Pushing command $cmd for sending upon connection"
	lappend CONN(queue) $cmd
	return 1
    }
    
    return 0
}


proc ::autoconnect::conninfo { whomto what } {
    variable AC
    variable log

    set url [__canonicalize $whomto]
    if { $url ne "" } {
	set cx [::uobj::find [namespace current] connection [list url == $url]]
	if { $cx ne "" } {
	    upvar \#0 $cx CONN

	    switch -- [string tolower $what] {
		"connection" {
		    return $CONN(id)
		}
		"url" {
		    return $CONN(url)
		}
		"framing" {
		    return $CONN(framing)
		}
		"proto" -
		"protocol" {
		    return $CONN(proto)
		}
		"state" {
		    switch -glob -- $CONN(proto) {
			"ws*" {
			    return [::websocket::conninfo $CONN(id) state]
			}
			"tcp" {
			    foreach {sock hst prt} [::permclient::info $CONN(id)] break
			    if { $sock ne "" } {
				return CONNECTED
			    } else {
				return CLOSED
			    }
			}
		    }
		}
	    }
	}
    }
    return ""
}


# ::autoconnect::hints -- Sending hints
#
#       Provide and store hints for remote URLs to which we will be sending
#       later on. All hints from all matching patterns will be combined and
#       taken into account. Hint patterns are guaranteed to be unique, and order
#       is guaranteed to be kept.
#
# Arguments:
#	ptn	Glob-style pattern to match against canonical URL
#       args	Dash-led options and arguments, passed to protocol-specific opening
#       	commands or interal to library 
#
# Results:
#       Return current hints for pattern passed as a parameter, if any.
#
# Side Effects:
#       None.
proc ::autoconnect::hints { ptn args } {
    variable AC
    variable log
    
    # Reconstruct the global hints so that: if there was already exactly the
    # same pattern, it is replaced by the new arguments. If there wasn't a
    # pattern, add it at the end. We do this through a temporary local list in
    # order to make sure that we keep the order of the patterns.
    set modified 0
    set hints {}
    foreach {p a} $AC(hints) {
	if { $p eq $ptn } {
	    lappend hints $ptn $args;  # Replace current
	    set modified 1
	} else {
	    lappend hints $p $a;       # Keep existing
	}
    }
    if { ! $modified } {
	lappend hints $ptn $args;      # Append if we had not changed
    }
    set AC(hints) $hints; # Now global list is same as local reconstructed list

    # Now return the current list of hints for the pattern. We are sure to hit
    # the pattern as the algorithm for replacement above ensures uniqueness of
    # patterns in the hints list. 
    foreach {p a} $AC(hints) {
	if { $p eq $ptn } {
	    return $a
	}
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

    set cx [__context $dest]
    if { $cx eq "" } {
	return ""
    }
    upvar \#0 $cx CONN
    
    # Generate a synchronise variable that the reception procedure
    # will use to mediate back the answer from the server.
    set id [incr AC(idgene)]
    set varname "::autoconnect::__sync_${id}"
    upvar \#0 $varname sync
    set sync ""

    lappend CONN(syncs) SET $varname

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
    after $tmout [namespace current]::__unblock $varname "$tmout_m"
    if { [eval [list [namespace current]::send $dest $cmd $sendargs]] } {
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
    set CONN(syncs) [lreplace $AC(syncs) [expr $idx - 1] $idx]

    return $res
}


package provide autoconnect 0.3
