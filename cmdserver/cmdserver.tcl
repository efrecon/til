# cmdserver.tcl --
#
#	This module implements a generic command server implementation
#	for implementing simple command line protocols.  The module
#	provides for basic security control through host allowance and
#	denial.  Apart for a few initialisations, calling modules will
#	simply have to provide one or several callbacks for the
#	implementation of command logic.
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

package require Tcl 8.2
package require logger
package require errhan
package require dnsresolv

package provide cmdserver 1.0

namespace eval ::cmdserver {
    # Variables of name ::cmdserver::__Server_<id> and
    # ::cmdserver::__Client are created as arrays to support each
    # server and each connection to a server.

    # Initialise global state
    variable CMDS
    if {![info exists CMDS]} {
	array set CMDS {
	    servers    ""
	    serv_id    0
	    err_cb     0
	    loglevel   warn
	}
	variable log [::logger::init cmdserver]
	${log}::setlevel $CMDS(loglevel)
    }

    namespace export new getcmds addcmds delcmds \
	name client_name restrict client_ioctl clients broadcast \
	remove_client takeover release delete \
	assocs assoc_get assoc_put connections_cb loglevel
}


# ::cmdserver::loglevel -- Set/Get current log level.
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
proc ::cmdserver::loglevel { { loglvl "" } } {
    variable CMDS
    variable log

    if { $loglvl != "" } {
	if { [catch "${log}::setlevel $loglvl"] == 0 } {
	    set CMDS(loglevel) $loglvl
	}
    }

    return $CMDS(loglevel)
}


# ::cmdserver::__writeline --
#
#	Write one line to a socket and flush
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
proc ::cmdserver::__writeline { sock line } {
    variable log

    # Log output
    set sockinfo [fconfigure $sock -sockname]
    ${log}::debug \
	"Writing to $sock ([lindex $sockinfo 1]:[lindex $sockinfo 2]): $line"

    # Write to socket and catch flushing.
    puts $sock $line
    set err [catch "flush $sock"]

    if { $err } {
	${log}::warn \
	    "Failed write to sock\
             ([lindex $sockinfo 1]:[lindex $sockinfo 2] @ $sock)"
    }

    return $err
}


# ::cmdserver::__error --
#
#	Procedure called by the error manager when an error has
#	occured.  We detect disappearance of client automatically and
#	remove them from our client list.
#
# Arguments:
#	err	Error code
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::cmdserver::__error { err } {
    set ok_error 0

    if { [string match "error *ing \"*\": broken pipe" $err] \
	     || [string match "error *ing \"*\": connection reset by peer" \
		     $err] } {
	variable CMDS
	variable log

	set open_quote [string first "\"" $err]
	incr open_quote
	set close_quote [string last "\"" $err]
	incr close_quote -1
	set sock [string range $err $open_quote $close_quote]

	set sockinfo [fconfigure $sock -sockname]
	${log}::notice \
	    "Broken connection to\
             [lindex $sockinfo 1]:[lindex $sockinfo 2] @ $sock detected"

	set ok_error [::cmdserver::remove_client $sock]
    }

    return $ok_error
}


# ::cmdserver::__read --
#
#	Read commands coming from remote clients, directly treat
#	standard commands and dispatch others accordingly
#
# Arguments:
#	servid	Identifier of server
#	sock	Socket to read commands from.
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::cmdserver::__read { servid sock } {
    variable CMDS
    variable log

    # Get information about server and client
    set varname "::cmdserver::__Server_$servid"
    upvar \#0 $varname Server
    set varname "::cmdserver::__Client_$sock"
    upvar \#0 $varname Client

    set quiting 0
    if { [catch "$Client(read_cmd) $sock cmd" len] != 0 } {
	${log}::warn "Could not read from remote client: $len"
	set quiting 1
	set cmd ""
    }
    if { $len < 0 } {
	set quiting 1
	set cmd ""
    }
    
    set uppercmd [string toupper $cmd]
    if { ! $quiting && $cmd != "" } {
	${log}::debug "Incoming command: $cmd"
    }

    set space [string first " " $uppercmd]
    if { $space < 0 } {
	set thecmd $uppercmd
    } else {
	set thecmd [string range $uppercmd 0 [expr $space - 1]]
    }

    switch $thecmd {
	QUIT {
	    set quiting 1
	}
	CLOSE {
	    set quiting 1
	}
	FINISH {
	    ${log}::notice "Closing server $Server(name)"
	    ::cmdserver::delete $servid
	}
	EXIT {
	    ${log}::notice "Exiting application..."
	    exit
	}
	COMMANDS {
	    if { [$Client(write_cmd) $sock \
		      [concat "COMMANDS QUIT CLOSE FINISH EXIT COMMANDS" \
			   $Server(cmds)]] } {
		set quiting 1
	    }
	}
	default {
	    if { $thecmd != "" } {
		set idx [lsearch $Server(cmds) $thecmd]
		if { $idx < 0 } {
		    set idx [lsearch $Server(cmds) "__DEFAULT__"]
		    if { $idx < 0 } {
			${log}::warn "$thecmd is an unrecognised command"
		    }
		}
	    
		if { $idx >= 0 } {
		    if { $space < 0 } {
			set remaining ""
		    } else {
			set remaining [string trim \
					   [string range $cmd $space end]]
		    }
		    if { [llength $Server(cbs)] == 1 } {
			if { [catch {eval $Server(cbs) $sock $thecmd \
					 [list $remaining]} err] } {
			    ${log}::warn "Error when invoking incoming cb\
                                          command $thecmd on server\
                                          '$Server(name)': $err"
			}
		    } else {
			if { [catch {eval [lindex $Server(cbs) $idx] $sock \
					 [list $remaining]} err] } {
			    ${log}::warn "Error when invoking incoming cb\
                                          command $thecmd on server\
                                          '$Server(name)': $err"
			}
		    }
		}
	    }
	}
    }

    # Nothing more to be done if the client already has disconnected
    # from the server in the mean time.  If the client is still there,
    # remove it if it has closed the connection.
    if { [lsearch $Server(clients) $sock] >= 0 } {
	if { [eof $sock] } {
	    set quiting 1
	}
	
	if { $quiting } {
	    ::cmdserver::remove_client $sock
	}
    }
}


# ::cmdserver::__newclient --
#
#	Accept/Reject connection from clients and register new client.
#	standard commands and dispatch others accordingly
#
# Arguments:
#	sock	Socket for connection with client.
#	ip	IP number of client
#	port	Port number for connection.
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::cmdserver::__newclient { servid sock ip port } {
    variable CMDS
    variable log

    # Get information about server
    set varname "::cmdserver::__Server_$servid"
    upvar \#0 $varname Server

    # Grant/Reject connection according to current restriction filters.
    set hostname $ip
    if { [llength $Server(allow)] > 0 || [llength $Server(deny)] > 0 } {
	if { [info commands ::dnsresolv::inverse] != "" } {
	    if { [catch {::dnsresolv::inverse $ip} hostname] } {
		set hostname ""
	    }
	    
	    if { $hostname == "" } {
		${log}::warn "Cannot discover host name of $ip"
		set hostname $ip
	    }
	}

	set allowed 0
	foreach ptn $Server(allow) {
	    if { [string match $ptn $ip] || [string match $ptn $hostname] } {
		set allowed 1
	    }
	}
	if { ! $allowed } {
	    foreach ptn $Server(deny) {
		if { [string match $ptn $ip] \
			 || [string match $ptn $hostname] } {
		    ${log}::notice "Client $ip/$hostname rejected!"
		    close $sock
		    return
		}
	    }
	}
    }
    
    # Remember the new client of the server
    # fconfigure $sock -buffering line
    lappend Server(clients) $sock

    # Remember information about the client.
    set varname "::cmdserver::__Client_$sock"
    upvar \#0 $varname Client
    set Client(sock) $sock
    set Client(ip) $ip
    set Client(hostname) $hostname
    set Client(port) $port
    set Client(server) $Server(id)
    set Client(external) 0
    set Client(read_cmd) "gets"
    set Client(write_cmd) "::cmdserver::__writeline"
    set Client(close_cmd) "close"
    foreach assoc $Server(assocs) {
	set Client($assoc) ""
    }
    fileevent $sock readable "::cmdserver::__read $servid $sock"
    ${log}::notice "$Server(name): New client $hostname:$port"

    # Give callback
    if { $Server(newclient_cb) != "" } {
	if { [catch {eval $Server(newclient_cb) $servid \
			 $sock $ip $port} err] } {
	    ${log}::warn "Error when invoking new client callback for\
                          '[client_name $sock]': $err"
	}
    }
}


# ::cmdserver::new --
#
#	Create a new command server and start accepting/rejecting
#	connection clients on port number.
#
# Arguments:
#	port	Port onto which we listen for connections
#	cmds	List of accepted commands, __default__ is a special command
#               and can be used to catch all non-recognised commands
#	cbs	List of callbacks for command reception (one cb is
#               same callback for all commands)
#	name	User leve name of command server 
#
# Results:
#	An identifier that will be used for all further operations on
#	server, negative number on error.
#
# Side Effects:
#	None.
proc ::cmdserver::new { port cmds cbs { name "" } { sockcmd "socket" } } {
    variable CMDS
    variable log

    set len [llength $cbs]
    if { $len > 1 && $len != [llength $cmds] } {
	${log}::error "Length of callbacks and commands list mismatch!"
	return -1
    }

    # Register a new server
    incr CMDS(serv_id)
    set varname "::cmdserver::__Server_$CMDS(serv_id)"
    upvar \#0 $varname Server
 
    # Remember information about the server
    lappend CMDS(servers) $CMDS(serv_id)
    set Server(id) $CMDS(serv_id)
    set Server(port) $port
    if { $name == "" } {
	set name "Server \#$Server(id) on port \#$Server(port)"
    }
    ::cmdserver::name $CMDS(serv_id) $name
    ${log}::notice \
	"Starting $Server(name), listening for connections on port: $port"
    set res [catch [list $sockcmd \
			-server "::cmdserver::__newclient $CMDS(serv_id)" \
			$port] Server(sock)]
    if { $res != 0 } {
	${log}::error "Cannot open server listening socket on port $port"
	set CMDS(servers) [lreplace $CMDS(servers) end end]
	unset Server
	return -1
    }
    set Server(clients) ""
    set Server(cmds) [string toupper $cmds]
    set Server(cbs) $cbs
    set Server(newclient_cb) ""
    set Server(delclient_cb) ""
    set Server(assocs) ""
    set Server(allow) {}
    set Server(deny) {}

    # Register a callback at the error handler to detect client disconnection
    if { ! $CMDS(err_cb) } {
	set CMDS(err_cb) 1
	::errhan::add ::cmdserver::__error
    }

    # return the identifier of the server
    return $CMDS(serv_id)
}


proc ::cmdserver::get { servid what } {
    variable CMDS
    variable log

    # Check that this is really one of our servers
    if { [lsearch -exact $CMDS(servers) $servid] < 0 } {
	${log}::error "Server $servid has disappeared"
	return ""
    }
 
    # Get to its information
    set varname "::cmdserver::__Server_$servid"
    upvar \#0 $varname Server

    switch -nocase -- $what {
	"socket" {
	    return $Server(sock)
	}
	"clients" {
	    return $Server($what)
	}
    }
    return ""
}

# ::cmdserver::getcmds --
#
#	Return the list of commands associated to a server, except
#	internal commands.
#
# Arguments:
#	servid	Identifier of server, as returned by ::cmdserver::new
#	ptn	Pattern that the command name has to match, defaults to all
#
# Results:
#	List of declared implemented commands.
#
# Side Effects:
#	None.
proc ::cmdserver::getcmds { servid { ptn "*" } } {
    variable CMDS
    variable log

    # Check that this is really one of our servers
    if { [lsearch -exact $CMDS(servers) $servid] < 0 } {
	${log}::error "Server $servid has disappeared"
	return ""
    }
 
    # Get to its information
    set varname "::cmdserver::__Server_$servid"
    upvar \#0 $varname Server

    # Get all commands that match the pattern passed as a parameter.
    set cmds ""
    foreach c $Server(cmds) {
	if { [string match -nocase $ptn $c] } {
	    lappend cmds [string toupper $c]
	}
    }

    return $cmds
}


# ::cmdserver::addcmds --
#
#	Append a list of commands to the ones that are recognised by a
#	currently existing server.
#
# Arguments:
#	servid	Identifier of server, as returned by ::cmdserver::new
#	cmds	List of commands to be added.
#
# Results:
#	List of currently declared implemented commands.
#
# Side Effects:
#	None.
proc ::cmdserver::addcmds { servid cmds } {
    variable  CMDS
    variable log

    # Check that this is really one of our servers
    if { [lsearch -exact $CMDS(servers) $servid] < 0 } {
	${log}::error "Server $servid has disappeared"
	return ""
    }
 
    # Get to its information
    set varname "::cmdserver::__Server_$servid"
    upvar \#0 $varname Server

    # Add each command in upper case if it does not already exist.
    foreach c $cmds {
	set c [string toupper $c]
	set idx [lsearch $Server(cmds) $c]
	if { $idx < 0 } {
	    lappend Server(cmds) $c
	} else {
	    ${log}::error "Command $cmd is already registered at $Server(name)"
	}
    }

    return $Server(cmds)
}


# ::cmdserver::delcmds --
#
#	Remove a list of commands from those that are currently
#	associated to a command server.  This does not include
#	internal commands.
#
# Arguments:
#	servid	Identifier of server, as returned by ::cmdserver::new
#	cmds	List of commands to be removed
#
# Results:
#	List of currently declared implemented commands.
#
# Side Effects:
#	None.
proc ::cmdserver::delcmds { servid { cmds ""} } {
    variable CMDS
    variable log

    # Check that this is really one of our servers
    if { [lsearch -exact $CMDS(servers) $servid] < 0 } {
	${log}::error "Server $servid has disappeared"
	return ""
    }
 
    # Get to its information
    set varname "::cmdserver::__Server_$servid"
    upvar \#0 $varname Server

    # Remove all commands that are passed as a parameter
    foreach c $cmds {
	set c [string toupper $c]
	set idx [lsearch $Server(cmds) $c]
	if { $idx < 0 } {
	    ${log}::error "Command $cmd does not exist at $Server(name)"
	} else {
	    set Server(cmds) [lreplace $Server(cmds) $idx $idx]
	}
    }

    return $Server(cmds)
}


# ::cmdserver::name --
#
#	Give a name to a command server if a non-empty name is passed
#	as argument. Return the name of the server, or an empty string
#	if this server is unknown.
#
# Arguments:
#	servid	Identifier of server, as returned by ::cmdserver::new
#	name	(new) name of server
#
# Results:
#	Return the name of the server, or an empty string if this
#	server is unknown.
#
# Side Effects:
#	None.
proc ::cmdserver::name { servid { name "" } } {
    variable CMDS
    variable log

    # Check that this is really one of our servers
    if { [lsearch -exact $CMDS(servers) $servid] < 0 } {
	${log}::error "Server $servid has disappeared"
	return ""
    }
 
    # Get to its information
    set varname "::cmdserver::__Server_$servid"
    upvar \#0 $varname Server

    # Remember information about the server
    if { $name != "" } {
	set Server(name) $name
    }

    return $Server(name)
}


# ::cmdserver::client_name --
#
#	Return a human readable string describing the client (and
#	possibly the server name)
#
# Arguments:
#	sock	Socket to client.
#	serv_nm	Should we include the server name also?
#
# Results:
#	Return the string describing the client
#
# Side Effects:
#	None.
proc ::cmdserver::client_name { sock { serv_nm 1 } } {
    set varname "::cmdserver::__Client_$sock"
    upvar \#0 $varname Client
    set c_name "$Client(hostname):$Client(port)"

    if { $serv_nm } {
	set varname "::cmdserver::__Server_$Client(server)"
	upvar \#0 $varname Server
	append c_name "@$Server(name)"
    }

    return $c_name
}


# ::cmdserver::restrict --
#
#	Setup restriction filters to only allow some clients to
#	connect to this server (the default is to allow any client for
#	connection).  The restriction filters are modelled after the
#	hosts.allow and hosts.deny access control files. Access will
#	be granted for any host which name or IP matches any filer
#	from the allow list. Access will be denied for any hot which
#	name or IP matches any filter from the deny list. Otherwise,
#	access will be granted. Both list are allowed to contained
#	wildcard characters compatible with the string match command.
#
# Arguments:
#	servid	Identifier of server
#	allow	List of "allow" hostname/IP filters
#	deny	List of "deny" hostname/IP filters
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::cmdserver::restrict { servid { allow {} } { deny {} } } {
    variable CMDS
    variable log

    # Check that this is really one of our servers
    if { [lsearch -exact $CMDS(servers) $servid] < 0 } {
	${log}::error "Server $servid has disappeared"
	return ""
    }
 
    # Get to its information
    set varname "::cmdserver::__Server_$servid"
    upvar \#0 $varname Server

    set Server(allow) $allow
    set Server(deny) $deny
}


# ::cmdserver::client_ioctl --
#
#	Specifies particular commands to read, write and close to and
#	from sockets with clients.
#
# Arguments:
#	sock	Socket to client.
#	rd_cmd	Command to read (should look like gets)
#	wt_cmd	Command to write (should look like puts/flush)
#	cs_cmd	Command to close (should look like close)
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::cmdserver::client_ioctl { sock { rd_cmd "" } { wt_cmd "" } { cs_cmd "" } } {
    set varname "::cmdserver::__Client_$sock"
    upvar \#0 $varname Client

    if { $rd_cmd == "" } {
	set Client(read_cmd) "gets"
    } else {
	set Client(read_cmd) $rd_cmd
    }
    if { $wt_cmd == "" } {
	set Client(write_cmd) "::cmdserver::__writeline"
    } else {
	set Client(write_cmd) $wt_cmd
    }
    if { $cs_cmd == "" } {
	set Client(close_cmd) "close"
    } else {
	set Client(close_cmd) $cs_cmd
    }
}


# ::cmdserver::clients --
#
#	Return list of clients for a given server.
#
# Arguments:
#	servid	Identifier of server, as returned by ::cmdserver::new
#
# Results:
#	List of known current clients.
#
# Side Effects:
#	None.
proc ::cmdserver::clients { servid } {
    variable CMDS
    variable log

    # Check that this is really one of our servers
    if { [lsearch -exact $CMDS(servers) $servid] < 0 } {
	${log}::error "Server $servid has disappeared"
	return ""
    }

    # Get to its information
    set varname "::cmdserver::__Server_$servid"
    upvar \#0 $varname Server

    # And return the list of clients.
    return $Server(clients)
}


# ::cmdserver::broadcast --
#
#	Broadcast a command to a number (or all) of clients of a server
#
# Arguments:
#	servid	Identifier of server, as returned by ::cmdserver::new
#	cmd	Command (line) to send to all clients.
#	clients	List of clients to send to.
#
# Results:
#	Number of clients to which the command was sent, negative on error.
#
# Side Effects:
#	None.
proc ::cmdserver::broadcast { servid cmd { clients "" } } {
    variable CMDS
    variable log

    # Check that this is really one of our servers
    if { [lsearch -exact $CMDS(servers) $servid] < 0 } {
	${log}::error "Server $servid has disappeared"
	return -1
    }

    # Get to its information
    set varname "::cmdserver::__Server_$servid"
    upvar \#0 $varname Server

    # Now do the broadcast, either to all clients or only to the subset
    # specified, in which case we check that these are our clients.
    if { $clients == "" } {
	set clients $Server(clients)
    }
    set receivers ""
    foreach sock $clients {
	set idx [lsearch -exact $Server(clients) $sock]
	if { $idx >= 0 } {
	    # Find information about the client.
	    set varname "::cmdserver::__Client_$sock"
	    upvar \#0 $varname Client
	    if { [$Client(write_cmd) $sock $cmd] } {
		::cmdserver::remove_client $sock
	    } else {
		lappend receivers $sock
	    }
	} else {
	    ${log}::error "Client $sock is not a client of $Server(name)"
	}
    }

    if { [llength $receivers] > 0 } {
	${log}::debug "Sent cmd \"$cmd\" to $receivers"
    }

    return [llength $receivers]
}


# ::cmdserver::remove_client --
#
#	Unregister one of the clients of one our servers and close
#	connection to this client.
#
# Arguments:
#	sock	Socket of client to be removed.
#
# Results:
#	1 on removal success, 0 otherwise.
#
# Side Effects:
#	None.
proc ::cmdserver::remove_client { sock } {
    variable CMDS
    variable log
    
    # Find information about the client.
    set varname "::cmdserver::__Client_$sock"
    if { [info vars $varname] == "" } {
	return 1;  # Already removed, do nothing!!
    }
    upvar \#0 $varname Client

    # Then about the server
    set varname "::cmdserver::__Server_$Client(server)"
    upvar \#0 $varname Server

    # Give callback
    if { $Server(delclient_cb) != "" } {
	if { [catch {eval $Server(delclient_cb) $Client(server) \
			 $sock $Client(ip) $Client(port)} err] } {
	    ${log}::warn "Error when invoking client removal callback for\
                          [client_name $sock]: $err"
	}
    }

    # Now we can start the process...
    set newclients ""

    # Parse all known clients of this server, looking for the one that matches
    # the incoming socket
    set toclose [list]
    foreach client $Server(clients) {
	if { $client == $sock } {
	    # Now remove the client from our list of known clients and
	    # clean up.
	    if { [array names Client close_cmd] eq "" } {
		lappend toclose "" $sock
	    } else {
		lappend toclose "$Client(close_cmd)" $sock
	    }
	    catch {unset Client}
	} else {
	    lappend newclients $client
	}
    }

    # Update the list of clients if necessary.
    if { [llength $toclose] > 0 } {
	set Server(clients) $newclients

	foreach {cmd sock} $toclose {
	    if { [catch "fconfigure $sock -peername" cfg] } {
		${log}::warn "Cannot get info for remote client"
		set host <unknown>
		set port <unknown>
	    } else {
		foreach {addr host port} $cfg break
	    }
	    if { $cmd ne "" } {
		catch {$cmd $sock}
	    }
	    ${log}::notice "${host}:${port} disconnected"
	}
    }

    return [expr [llength $toclose] > 0]
}


# ::cmdserver::takeover --
#
#	Take over reading from a given socket associated to a client
#	of a server Control can be release later on.  This can be used
#	to read events that are contained on several lines, where only
#	the first line is a command.
#
# Arguments:
#	sock	Socket to client
#
# Results:
#	None.
#
# Side Effects:
#	No incoming command will be read and analysed further.
proc ::cmdserver::takeover { sock } {
    set varname "::cmdserver::__Client_$sock"
    upvar \#0 $varname Client

    set Client(external) 1
    fileevent $sock readable ""
}


# ::cmdserver::release --
#
#	Release back the control of a socket at the server. Control
#	can be taken over later on.
#
# Arguments:
#	sock	Socket to client
#
# Results:
#	None.
#
# Side Effects:
#	Incoming commands will be treated through cmdserver again.
proc ::cmdserver::release { sock } {
    set varname "::cmdserver::__Client_$sock"
    upvar \#0 $varname Client

    if { $Client(external) } {
	set Client(external) 0
	fileevent $sock readable "::cmdserver::__read $Client(server) $sock"
    }
}


# ::cmdserver::delete --
#
#	Remove one of our servers
#
# Arguments:
#	servid	Identifier of server, as returned by ::cmdserver::new
#
# Results:
#	1 or 0 to tell success or failure
#
# Side Effects:
#	All existing connections to all clients will be closed!
proc ::cmdserver::delete { servid } {
    variable CMDS
    variable log

    # Check that this is really one of our servers
    if { [lsearch -exact $CMDS(servers) $servid] < 0 } {
	${log}::error "Server $servid has disappeared"
	return 0
    }

    # Get to its information
    set varname "::cmdserver::__Server_$servid"
    upvar \#0 $varname Server

    # Remove and disconnect all its clients.
    foreach sock $Server(clients) {
	::cmdserver::remove_client $sock
    }

    # Then close listening connection and forget about this server
    ${log}::notice "Server $Server(name) terminated"
    catch "close $Server(sock)"
    unset Server
    set CMDS(servers) [lreplace $CMDS(servers) $idx $idx]

    return 1
}


# ::cmdserver::assocs --
#
#	Declare association that will be associated to each new
#	client.  These associations can be operated upon using
#	::cmdserver::assoc_put and ::cmdserver::assoc_get
#
# Arguments:
#	servid	Identifier of server, as returned by ::cmdserver::new
#	keys	Associated keys
#
# Results:
#	None
#
# Side Effects:
#	None.
proc ::cmdserver::assocs { servid assocs } {
    variable CMDS
    variable log

    # Check that this is really one of our servers
    if { [lsearch -exact $CMDS(servers) $servid] < 0 } {
	${log}::error "Server $servid has disappeared"
	return ""
    }

    # Get to its information
    set varname "::cmdserver::__Server_$servid"
    upvar \#0 $varname Server

    # Remember associations and count
    set nb_assocs 0
    foreach assoc $assocs {
	set idx [lsearch $Server(assocs) $assoc]
	if { $idx >= 0 } {
	    ${log}::info "\"$assoc\" already declared within \"$Server(name)\""
	} else {
	    # Remember this association since it does not already exists
	    lappend Server(assocs) $assoc
	    incr nb_assocs

	    # Initialise it for all current known clients.
	    foreach sock $Server(clients) {
		set varname "::cmdserver::__Client_$sock"
		upvar \#0 $varname Client
		set Client($assoc) ""
	    }
	}
    }
}


# ::cmdserver::assoc_get --
#
#	Get value associated to a key for a given client.
#
# Arguments:
#	sock	Socket to client.
#	assoc	Association key
#
# Results:
#	Current value associate to client under key.
#
# Side Effects:
#	None.
proc ::cmdserver::assoc_get { sock assoc } {
    set varname "::cmdserver::__Client_$sock"
    upvar \#0 $varname Client
    
    return [set Client($assoc)]
}


# ::cmdserver::assoc_put --
#
#	Put an association for a given client.
#
# Arguments:
#	sock	Socket to client.
#	assoc	Association key
#	value	New value for association
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::cmdserver::assoc_put { sock assoc value } {
    set varname "::cmdserver::__Client_$sock"
    upvar \#0 $varname Client
    
    set Client($assoc) $value
}


# ::cmdserver::connections_cb --
#
#	Register two procedures that will (possibly) called when new
#	clients connect and clients disconnect.
#
# Arguments:
#	servid	Identifier of server, as returned by ::cmdserver::new
#	newcli	Procedure to be called on new clients.
#	delcli	Procedure to be called when clients disconnect
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::cmdserver::connections_cb { servid newcli delcli } {
    variable CMDS
    variable log

    # Check that this is really one of our servers
    if { [lsearch -exact $CMDS(servers) $servid] < 0 } {
	${log}::error "Server $servid has disappeared"
	return ""
    }

    # Get to its information
    set varname "::cmdserver::__Server_$servid"
    upvar \#0 $varname Server

    set Server(newclient_cb) "$newcli"
    set Server(delclient_cb) "$delcli"
}
