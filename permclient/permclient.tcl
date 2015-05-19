# permclient.tcl --
#
#	This module ensures to keep a connection to a remote server
#	opened and alive all the time.  It is architected around two
#	core ideas: writing to the socket should be made through this
#	module and reading from the socket is handled internally and
#	data read is handled through a caller provided callback.  This
#	library is line-communication oriented.
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

package require Tcl 8.2
package require logger

package provide permclient 1.0

namespace eval ::permclient {
    # Variables of name ::permclient::__PermClient_<id> are created as
    # arrays to support each permanent connection to a server.

    # Initialise global state
    variable PermC
    if {![info exists PermC]} {
	array set PermC {
	    clients    ""
	    client_id  0
	    -open      ""
	    -poll      60
	    -block     -1
	    -proxy     ""
	    -down      ""
	    -socketcmd "socket"
	    loglevel   warn
	    proxyseps  "|:/ \t"
	}
	variable log [::logger::init permclient]
	${log}::setlevel $PermC(loglevel)
    }

    namespace export info new find ioctl delete write takeover release \
	loglevel waitalive
}




# ::permclient::__sockwrite --
#
#	Write to a socket, nicely catching errors.
#
# Arguments:
#	sock	Socket to write to
#	line	Text line to write to socket
#
# Results:
#	Return 0 if no error occured, a positive number otherwise.
#
# Side Effects:
#	None.
proc ::permclient::__sockwrite { sock line } {
    variable log

    # Log output
    set sockinfo [fconfigure $sock -sockname]
    ${log}::debug \
	"Writing to $sock ([lindex $sockinfo 1]:[lindex $sockinfo 2]): $line"
    
    # Write to socket and catch flushing.
    puts $sock $line
    set err [catch "flush $sock" errmsg]

    if { $err } {
	${log}::warn \
	    "Failed write to sock\
             ([lindex $sockinfo 1]:[lindex $sockinfo 2] @ $sock)"
    }

    return $err
}



# ::permclient::__read --
#
#	Something is ready to be read from the connection.  Read,
#	check that we can, reschedule connection opening if it was
#	shutdown, otherwise deliver callback with content.
#
# Arguments:
#	id	Identifier of this client connection
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::permclient::__read { id } {
    variable PermC
    variable log

    # Check that this is one of our known connections.
    if { [lsearch -exact $PermC(clients) $id] < 0 } {
        ${log}::warn "Client $id is not registered"
	return ""
    }

    # Get to information on connection and reinitialise socket 
    set varname "::permclient::__PermClient_$id"
    upvar \#0 $varname Client

    set res [::permclient::gets $id line]
    if { $res > 0 && $Client(read_cb) != ""} {
	if { [catch {eval $Client(read_cb) $Client(id) \$line} err] } {
	    ${log}::warn "Error when invoking incoming socket\
                          callback for client $id (to: \
                          $Client(host):$Client(port)): $err"
	}
    }
}


# ::permclient::__connect --
#
#	Connect to a remote server, reschedule a connection opening if
#	we cannot establish the connection.  Callback the connection
#	opening callback if we can and make sure that we register for
#	one of our procedure whenever something can be read from the
#	socket.
#
# Arguments:
#	id	Identifier of this client connection.
#	again	Should we try again at a later time?
#
# Results:
#	The socket to the server, or an empty string in case of error.
#
# Side Effects:
#	None.
proc ::permclient::__connect { id { again 1 } } {
    variable PermC
    variable log

    # Check that this is one of our known connections.
    if { [lsearch -exact $PermC(clients) $id] < 0 } {
	${log}::warn "Client $id is not registered" 1
	return ""
    }

    # Get to information on connection and reinitialise socket 
    set varname "::permclient::__PermClient_$id"
    upvar \#0 $varname Client
    set Client(sock) ""

    # Now, try to open the socket connection.
    if { $Client(-proxy) == "" } {
	${log}::debug "Trying to connect to $Client(host):$Client(port)"
	set host $Client(host)
	set port $Client(port)
    } else {
	${log}::debug "Trying to connect to $Client(host):$Client(port) via\
                       $Client(-proxy)"
	set pxy [split $Client(-proxy) $PermC(proxyseps)]
	set host [lindex $pxy 0]
	set port [lindex $pxy 1]
    }
    set res [catch "$Client(-socketcmd) $host $port" sock]

    if { $res == 0 } {
	# We could, deliver connection opening callback and make sure we
	# will read from the socket whenever something exists there.
	set Client(sock) $sock
	set Client(schedule) ""
	# set Client(external) 0
	if { $Client(-proxy) != "" } {
	    # Fool ourselves a bit, see to talk directly to the proxy,
	    # ask it toconnect, then get back to normal.
	    set proxy $Client(-proxy)
	    set Client(-proxy) ""
	    ::permclient::write $id "CONNECT $Client(host)/$Client(port)"
	    set Client(-proxy) $proxy
	}
	if { ! $Client(external) } {
	    fileevent $sock readable "::permclient::__read $id"
	}
	${log}::notice \
	    "Connection with $Client(host):$Client(port) established"
	if { $Client(-open) != "" } {
	    if { [catch {eval $Client(-open) $id $sock} err] } {
		${log}::warn "Error when invoking open callback for\
                              client $id (to: \
                              $Client(host):$Client(port)): $err"
	    }
	}
    } elseif { $again } {
	# We could not, reschedule another try later on.
	${log}::warn "Server at $Client(host):$Client(port) not alive,\
                      rescheduling opening of connection in\
                      $Client(-poll) seconds"
	if { $Client(-down) != "" } {
	    if { [catch {eval $Client(-down) $id} err] } {
		${log}::warn "Error when invoking down callback for\
                              client $id (to: \
                              $Client(host):$Client(port)): $err"
	    }
	}
	set Client(schedule) \
	    [after [expr $Client(-poll) * 1000] "::permclient::__connect $id"]
    }

    # Return the socket for this connection.
    return $Client(sock)
}


# ::permclient::loglevel -- Set/Get current log level.
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
proc ::permclient::loglevel { { loglvl "" } } {
    variable PermC
    variable log

    if { $loglvl != "" } {
	if { [catch "${log}::setlevel $loglvl"] == 0 } {
	    set PermC(loglevel) $loglvl
	}
    }

    return $PermC(loglevel)
}



# ::permclient::info --
#
#	Identify a permanent client connection and return connection
#	information.
#
# Arguments:
#	id	Identifier of the connection
#
# Results:
#	Returns a list composed of the socket to communicate with the
#	server, the host and the port. Empty list if not found.
#
# Side Effects:
#	None.
proc ::permclient::info { id } {
    variable PermC
    variable log

    # Check that this is one of our known connections.
    if { [lsearch -exact $PermC(clients) $id] < 0 } {
	${log}::warn "Client $id is not registered"
	return ""
    }

    # Get to information on connection and return information
    set varname "::permclient::__PermClient_$id"
    upvar \#0 $varname Client

    return [list "$Client(sock)" $Client(host) $Client(port)]
}



# ::permclient::reconnect -- Force a reconnection
#
#	Force a manual reconnection to the server by closing the
#	socket and reopening the connection once.
#
# Arguments:
#	id	Identifier of the connection
#
# Results:
#	Returns the (new) socket for that connection
#
# Side Effects:
#	None.
proc ::permclient::reconnect { id } {
    variable PermC
    variable log

    # Check that this is one of our known connections.
    if { [lsearch -exact $PermC(clients) $id] < 0 } {
	${log}::warn "Client $id is not registered"
	return ""
    }

    # Get to information on connection and return information
    set varname "::permclient::__PermClient_$id"
    upvar \#0 $varname Client

    if { $Client(schedule) != "" } {
	after cancel $Client(schedule)
	set Client(schedule) ""
    }
    if { $Client(sock) != "" } {
	fileevent $Client(sock) readable ""
	catch "$Client(close_cmd) $Client(sock)"
	set Client(sock) ""
    }
    return [__connect $id]
}



# ::permclient::new --
#
#	Create a new permanent client connection to a server and
#	return a reference to it.  The recognised additional options
#	are -open -poll -block -proxy.
#
# Arguments:
#	host	Name (or IP) of host to connect to
#	port	Port of host to connect to
#	read_cb	Callback to call everytime a line has been read.
#	args	List of additional options.
#
# Results:
#	Return the identifier of the connection to be used in all
#	further calls to this library.
#
# Side Effects:
#	None.
proc ::permclient::new { host port read_cb args } {
    variable PermC
    variable log

    incr PermC(client_id)
    set varname "::permclient::__PermClient_$PermC(client_id)"
    upvar \#0 $varname Client

    set Client(id) $PermC(client_id)
    set Client(host) $host
    set Client(port) $port
    set Client(read_cb) $read_cb
    set Client(read_cmd) "::gets"
    set Client(write_cmd) "::permclient::__sockwrite"
    set Client(close_cmd) "::close"
    set Client(sock) ""
    set Client(external) 0
    foreach opt [array names PermC "-*"] {
	set Client($opt) $PermC($opt)
    }
    lappend PermC(clients) $Client(id)
    eval config $Client(id) $args
    
    set Client(schedule) [after idle "::permclient::__connect $Client(id)"]
    if { $Client(-block) >= 0 } {
	waitalive $Client(id) $Client(-block)
    }

    return $Client(id)
}


# ::permclient::config -- Configure a client connection
#
#	This command set or get the options of a client connection.
#
# Arguments:
#	id	Connection identifier
#	args	list of options
#
# Results:
#	Return all options, the option requested or set the options
#
# Side Effects:
#	None.
proc ::permclient::config { id args } {
    variable PermC
    variable log

    # Check that this is one of our known connections.
    set idx [lsearch -exact $PermC(clients) $id]
    if { $idx < 0 } {
	${log}::warn "Client $id is not registered"
	return -code error "Identifier invalid"
    }


    set varname "::permclient::__PermClient_${id}"
    upvar \#0 $varname Client

    set o [lsort [array names Client "-*"]]

    if { [llength $args] == 0 } {      ;# Return all results
	set result ""
	foreach name $o {
	    lappend result $name $Client($name)
	}
	return $result
    }

    foreach {opt value} $args {        ;# Get onr or set some
	if { [lsearch $o $opt] == -1 } {
	    return -code error "Unknown option $opt, must be: [join $o ", " ]"
	}
	if { [llength $args] == 1 } {  ;# Get one config value
	    return $Client($opt)
	}
	set Client($opt) $value         ;# Set the config value
    }
}


# ::permclient::__wakeup -- Wakeup a waiting caller
#
#	This command arranges to wake up a caller that is waiting for
#	the establishment of the connection through waitalive.
#
# Arguments:
#	id	Identifier of the connection to the client
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::permclient::__wakeup { id } {
    variable PermC
    variable log

    # Check that this is one of our known connections.
    set idx [lsearch -exact $PermC(clients) $id]
    if { $idx < 0 } {
	${log}::warn "Client $id is not registered"
	return ""
    }

    # Get to information on connection and reinitialise socket 
    set varname "::permclient::__PermClient_$id"
    upvar \#0 $varname Client

    if { $Client(sock) == "" } {
	set Client(sock) "__ARTIFICIAL_WAKEUP__"
    }
}


# ::permclient::waitalive -- Wait until server is alive
#
#	Wait the caller until the remote server is alive (again)
#
# Arguments:
#	id	Identifier of the connection to the client
#	timeout	Max number of milliseconds before we timeout
#
# Results:
#	Return the socket for communication to the server, or an empty string.
#
# Side Effects:
#	None.
proc ::permclient::waitalive { id { timeout -1 } } {
    variable PermC
    variable log

    # Check that this is one of our known connections.
    set idx [lsearch -exact $PermC(clients) $id]
    if { $idx < 0 } {
	${log}::warn "Client $id is not registered"
	return ""
    }

    # Get to information on connection and reinitialise socket 
    set varname "::permclient::__PermClient_$id"
    upvar \#0 $varname Client

    if { $timeout >= 0 } {
	after $timeout "::permclient::__wakeup $id"
    }

    while { $Client(sock) == "" } {
	${log}::debug "Waiting for connection to $Client(host):$Client(port)"
	vwait ${varname}(sock)
	if { $Client(sock) == "__ARTIFICIAL_WAKEUP__" } {
	    ${log}::info \
		"Timeout reached when waiting for connection to\
                 $Client(host):$Client(port)"
	    set Client(sock) ""
	    break
	}
    }

    return $Client(sock)
}


# ::permclient::find --
#
#	Find the permanent client identifier that match a given host
#	and port (there might be more than one).
#
# Arguments:
#	host	string match like pattern for name of host
#	port	string match like pattern for port of service at host
#
# Results:
#	Return a list of matching connection identifiers or an empty list.
#
# Side Effects:
#	None.
proc ::permclient::find { host port } {
    variable PermC

    set matchs ""

    foreach id $PermC(clients) {
	# Get to information on connection and reinitialise socket 
	set varname "::permclient::__PermClient_$id"
	upvar \#0 $varname Client

	if { [string match -nocase $host $Client(host)] \
		 && [string match $port $Client(port)] } {
	    lappend matchs $id
	}
    }

    return $matchs
}




# ::permclient::ioctl --
#
#	Allow the specification of specific functions to read and
#	write to the socket
#
# Arguments:
#	id	Identifier of the connection to the client
#	rd_cmd	Read command (will look like puts)
#	wr_cmd	Write command (will look like gets)
#	cls_cmd	Close command (will look like close)
#
# Results:
#	A triple containing, respectively, the read, the write and the
#	close command that are used on sockets opened and handled
#	through this module, or an empty list.
#
# Side Effects:
#	None.
proc ::permclient::ioctl { id { rd_cmd "" } { wr_cmd "" } { cls_cmd "" } } {
    variable PermC
    variable log

    # Check that this is one of our known connections.
    set idx [lsearch -exact $PermC(clients) $id]
    if { $idx < 0 } {
	${log}::warn "Client $id is not registered"
	return ""
    }

    # Get to information on connection and reinitialise socket 
    set varname "::permclient::__PermClient_$id"
    upvar \#0 $varname Client

    if { $rd_cmd == "" } {
	set Client(read_cmd) "gets"
    } else {
	set Client(read_cmd) $rd_cmd
    }
    if { $wr_cmd == "" } {
	set Client(write_cmd) "::permclient::__sockwrite"
    } else {
	set Client(write_cmd) $wr_cmd
    }
    if { $cls_cmd == "" } {
	set Client(close_cmd) "close"
    } else {
	set Client(close_cmd) $cls_cmd
    }

    return [list $Client(read_cmd) $Client(write_cmd) $Client(close_cmd)]
}



# ::permclient::delete --
#
#	Delete an existing permanent connection, disconnect and remove
#	any reference to it.
#
# Arguments:
#	id	Identifier of this client connection.
#
# Results:
#	Return 1 on success, 0 on failure.
#
# Side Effects:
#	All references to this connection are lost and further calls
#	to this library with this identifier will lead to errors.
proc ::permclient::delete { id } {
    variable PermC
    variable log

    # Check that this is one of our known connections.
    set idx [lsearch -exact $PermC(clients) $id]
    if { $idx < 0 } {
	${log}::warn "Client $id is not registered"
	return 0
    }

    # Get to information on connection and reinitialise socket 
    set varname "::permclient::__PermClient_$id"
    upvar \#0 $varname Client

    if { $Client(schedule) != "" } {
	after cancel $Client(schedule)
    }
    if { $Client(sock) != "" } {
	fileevent $Client(sock) readable ""
	catch "$Client(close_cmd) $Client(sock)"
    }
    set PermC(clients) [lreplace $PermC(clients) $idx $idx]
    unset Client

    return 1
}



# ::permclient::write --
#
#	Send one line to a remote server, flush the socket once the
#	line has been output.  This command handles errors when
#	writing to the connection socket nicely.  It detects failures
#	and will schedule a periodical re-opening of the connection
#	until it works.  While the connection is not opened, writing
#	will return 0.
#
# Arguments:
#	id	Identifier of this client connection
#	line	Line to be sent to the remote server
#
# Results:
#	Returns 1 on success, 0 on failure.
#
# Side Effects:
#	None.
proc ::permclient::write { id line } {
    variable PermC
    variable log

    # Check that this is one of our known connections.
    set idx [lsearch -exact $PermC(clients) $id]
    if { $idx < 0 } {
	${log}::warn "Client $id is not registered"
	return 0
    }

    # Get to information on connection
    set varname "::permclient::__PermClient_$id"
    upvar \#0 $varname Client

    if { $Client(sock) == "" } {
	${log}::warn "Server at $Client(host):$Client(port) is not alive,\
                      cannot send"
	# No server on the other end.
	return 0
    }

    ${log}::debug "Sending to $Client(host):$Client(port): $line"
    if { $Client(-proxy) == "" } {
	set out $line
    } else {
	set out "$Client(host)/$Client(port) $line"
    }
    set err [$Client(write_cmd) $Client(sock) $out]
    if { $err } {
	fileevent $Client(sock) readable ""
	catch "$Client(close_cmd) $Client(sock)"
	set Client(sock) ""
	${log}::notice "Server at $Client(host):$Client(port) has died,\
                        rescheduling opening of connection in\
                        $Client(-poll) seconds"
	if { $Client(-down) != "" } {
	    if { [catch {eval $Client(-down) $id} err] } {
		${log}::warn "Error when invoking down callback for\
                              client $id (to: \
                              $Client(host):$Client(port)): $err"
	    }
	}
	set Client(schedule) \
	    [after [expr $Client(-poll) * 1000] "::permclient::__connect $id"]
    }

    return [expr ! $err]
}



# ::permclient::gets -- Get a line from a (taken over) client.
#
#	This command get a line from a taken over client, while still
#	providing some level of error handling (as opposed to reading
#	directly from the socket sent back by ::permclient::takeover).
#
# Arguments:
#	id	Identifier of this client connection
#	line_p	Pointer to the variable that will contain the line read
#
# Results:
#	If a variable is provided, returns -1 on error or the number
#	of characters read.  If no variable is provided, returns the
#	next line read from the socket or an empty string.
#
# Side Effects:
#	None.
proc ::permclient::gets { id { line_p "" } } {
    variable PermC
    variable log

    if { $line_p != "" } {
	upvar $line_p line
    }
    set line ""

    # Check that this is one of our known connections.
    set idx [lsearch -exact $PermC(clients) $id]
    if { $idx < 0 } {
	${log}::warn "Client $id is not registered"
	if { $line_p == "" } {
	    return ""
	} else {
	    return -1
	}
    }

    # Get to information on connection
    set varname "::permclient::__PermClient_$id"
    upvar \#0 $varname Client

    if { $Client(sock) == "" } {
	${log}::warn "Server is at $Client(host):$Client(port) not alive,\
                      cannot read"
	# We do not have a socket for the connection, make sure we schedule
	# one soon.
	${log}::notice "No connection with $Client(host):$Client(port),\
                        scheduling opening of connection in\
                        $Client(-poll) seconds"
	set Client(schedule) \
	    [after [expr $Client(-poll) * 1000] "::permclient::__connect $id"]
	# Return viable result.
	if { $line_p == "" } {
	    return ""
	} else {
	    return -1
	}
    }

    if { [catch {$Client(read_cmd) $Client(sock) line} res] } {
	${log}::notice "Failed reading from socket: $res"
	set res -1
    }
    if { $res == -1 } {
	# We have received a read error, which means that connection
	# has been closed on the other end.  Reschedule an opening
	catch "fileevent $Client(sock) readable \"\""
	catch "$Client(close_cmd) $Client(sock)"
	set Client(sock) ""
	${log}::notice "Server at $Client(host):$Client(port) has died,\
                        rescheduling opening of connection in\
                        $Client(-poll) seconds"
	if { $Client(-down) != "" } {
	    if { [catch {eval $Client(-down) $id} err] } {
		${log}::warn "Error when invoking down callback for\
                              client $id (to: \
                              $Client(host):$Client(port)): $err"
	    }
	}
	set Client(schedule) \
	    [after [expr $Client(-poll) * 1000] "::permclient::__connect $id"]
	if { $line_p == "" } {
	    return ""
	} else {
	    return -1
	}
    } else {
	# We have read something, it is not an empty line, fix the
	# line if we are talking to a proxy and return it.
	if { $line != "" } {
	    if { $Client(-proxy) != "" } {
		set spaceidx [string first " " $line]
		set src [string trimright [string range $line 0 $spaceidx]]
		set line [string trimleft [string range $line $spaceidx end]]
	    }
	}
	if { $line_p == "" } {
	    return $line
	} else {
	    return [string length $line]
	}
    }
}



# ::permclient::takeover --
#
#	Take over a client connection socket so that further commands
#	will read themselves and directly.  The socket for reading is
#	returned and it will be up to the caller to handle errors
#	until ::permclient::release is called.  Some error handling
#	can be achieved through using ::permclient::gets.
#
# Arguments:
#	id	Identifier of the client connection
#
# Results:
#	Return the socket to reading directly from or an empty string
#	if the connection is unknown.
#
# Side Effects:
#	If failure occurs when reading is handled outside of the
#	library, the library will not be able to detect errors and
#	schedule reconnection until it is put back into control of the
#	connection.
proc ::permclient::takeover { id } {
    variable PermC
    variable log

    # Check that this is one of our known connections.
    set idx [lsearch -exact $PermC(clients) $id]
    if { $idx < 0 } {
	${log}::warn "Client $id is not registered"
	return ""
    }

    # Get to information on connection and reinitialise socket 
    set varname "::permclient::__PermClient_$id"
    upvar \#0 $varname Client

    set Client(external) 1
    fileevent $Client(sock) readable ""

    return $Client(sock)
}



# ::permclient::release --
#
#	Release a client connection socket so that this module takes
#	care of reading and error handling again.
#
# Arguments:
#	id	Identifier of the client connection
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::permclient::release { id } {
    variable PermC
    variable log

    # Check that this is one of our known connections.
    set idx [lsearch -exact $PermC(clients) $id]
    if { $idx < 0 } {
	${log}::warn "Client $id is not registered"
	return ""
    }

    # Get to information on connection and reinitialise socket 
    set varname "::permclient::__PermClient_$id"
    upvar \#0 $varname Client

    if { $Client(external) } {
	set Client(external) 0
	fileevent $Client(sock) readable "::permclient::__read $id"
    }
}



# ::permclient::defaults -- Set/Get defaults for all new connections
#
#	This command sets or gets the defaults opetions for all new
#	connections, it will not perpetrate on existing pending
#	connections, use ::permclient::config instead.
#
# Arguments:
#	args	List of -key value or just -key to get value
#
# Results:
#	Return all options, the option requested or set the options
#
# Side Effects:
#	None.
proc ::permclient::defaults { args } {
    variable PermC
    variable log

    set o [lsort [array names PermC "-*"]]

    if { [llength $args] == 0 } {      ;# Return all results
	set result ""
	foreach name $o {
	    lappend result $name $PermC($name)
	}
	return $result
    }

    foreach {opt value} $args {        ;# Get onr or set some
	if { [lsearch $o $opt] == -1 } {
	    return -code error "Unknown option $opt, must be: [join $o ,]"
	}
	if { [llength $args] == 1 } {  ;# Get one config value
	    return $PermC($opt)
	}
	set PermC($opt) $value           ;# Set the config value
    }
}
