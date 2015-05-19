# param.tcl --
#
#	This module a client to the parameter server, with automatic
#       updating of the parameters.
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

package require Tcl 8.2
package require logger
package require permclient

package provide param 1.0

namespace eval ::param {
    # Initialise global state
    variable PC
    if { ![info exists PC]} {
	array set PC {
	    servers    ""
	    loglevel   warn
	}
	variable log [::logger::init ::param]
	${log}::setlevel $PC(loglevel)
    }


    namespace export open alive get set close
}

# ::param::loglevel -- Set/Get current log level.
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
proc ::param::loglevel { { loglvl "" } } {
    variable PC
    variable log

    if { $loglvl != "" } {
	if { [catch "${log}::setlevel $loglvl"] == 0 } {
	    ::set PC(loglevel) $loglvl
	}
    }

    return $PC(loglevel)
}

# ::param::__receive -- Receive and dispatch data from remote server
#
#	Receive information from a parameter server, dispatch and
#	handle it through updating our cached vision of the parameters
#	at the server, etc.
#
# Arguments:
#	id	Identifier of connection to server
#	line	Data sent by server.
#
# Results:
#	None
#
# Side Effects:
#	None.
proc ::param::__receive { id line } {
    variable PC
    variable log

    ::set varname "::param::Cache_$id"
    upvar \#0 $varname Params

    ::set cmd [lindex $line 0]
    if { $cmd == "SET" } {
	::set remaining [string trim \
			   [string range $line [string length $cmd] end]]

	::set len [llength $remaining]
	::set i 0
	while { $i < $len } {
	    ::set param_name [lindex $remaining $i]
	    incr i
	    ::set param_value [lindex $remaining $i]
	    incr i
	    ::set Params($param_name) $param_value
	}

	${log}::debug "Current cache: [array get Params]"
    } elseif { $cmd == "UNKNOWN" || $cmd == "UNSET" } {
	::set remaining [string trim \
			   [string range $line [string length $cmd] end]]

	foreach name $remaining {
	    if { [info vars $varname] != "" \
		     && [array names Params $name] != "" } {
		# Should be [array names Params -exact $name] but does
		# not work with older versions of Tcl.
		unset Params($name)
	    }
	}

	${log}::debug "Current cache: [array get Params]"
    }
}


# ::param::__open_connection -- Initialise connection
#
#	Initialise connection with parameter server through requesting
#       all parameters.
#
# Arguments:
#	id	Identifier of connection to server.
#	sock	Socket to communicate to server.
#
# Results:
#	None
#
# Side Effects:
#	Actively request all data from server.
proc ::param::__open_connection { id sock } {
    variable PC
    variable log

    # Initalise cache and information about remote server.
    ::set varname "::param::Server_$id"
    upvar \#0 $varname Server
    if { [info vars $varname] == "" } {
	::set Server(watchs) ""
    }

    ::set varname "::param::Cache_$id"
    upvar \#0 $varname Cache
    if { [info vars $varname] == "" } {
	array set $varname {}
    }

    # Force arrival of all values to us and reinstall parameter watches.
    ${log}::info "(Re)synchronising with param server state"
    ::permclient::write $id "GET *"
    
    if { $Server(watchs) != "" } {
	::permclient::write $id $Server(watchs)
    }
}


# ::param::new -- Open connection to remote server
#
#	Open connection to a parameter server.
#
# Arguments:
#	hstnm	Name of host on which the server runs
#	port	Port number of server
#	block	Block until connection is opened?
#	keepalive	Number of minutes to poll for server existence
#
# Results:
#	Return an identifier for the connection that will be used in
#       all further calls.
#
# Side Effects:
#	None.
proc ::param::open { { hstnm "localhost" } { port 3272 } { block 0 } { keepalive 1 } } {
    variable log
    variable PC
    
    ::set id [::permclient::find $hstnm $port]
    if { $id < 0 } {
	::set id [::permclient::new $hstnm $port \
		      ::param::__receive -open ::param::__open_connection \
		      -poll [expr $keepalive * 60]]
	if { $block } {
	    ::permclient::waitalive $id
	}
    }

    return $id
}


# ::param::alive -- Remote server alive?
#
#	Identify if the remote server is alive or not.  Nothing is
#	actively polled.
#
# Arguments:
#	id	Identifier of connection to server.
#
# Results:
#	1 if the server is alive, 0 otherwise.
#
# Side Effects:
#	None.
proc ::param::alive { id } {
    ::set nfo [::permclient::info $id]
    if { [lindex $nfo 0] == "" } {
	return 0
    } else {
	return 1
    }
}


# ::param::get -- Get a parameter
#
#	(actively) get the value of a remote parameter.
#
# Arguments:
#	id	Identifier of connection to server.
#	name	Name of remote parameter to get, will be watched and cached
#	block	Blocking call, if necessary
#
# Results:
#	The value of the remote parameter.
#
# Side Effects:
#	Actively talks to the server, and will establish a watch on the
#       parameter.
proc ::param::get { id name { block 1 } } {
    variable log

    ::set parname "::param::Cache_$id"
    upvar \#0 $parname Params

    if { [info vars $parname] != "" \
	     && [array names Params $name] != "" } {
	# should be [array names Params -exact $name] But it does not
	# work with older versions of Tcl.
	${log}::debug \
	    "Getting $name, which is in cache with value $Params($name)"
	return $Params($name)
    } else {
	::set varname "::param::Server_$id"
	upvar \#0 $varname Server

	::set idx [lsearch -exact $Server(watchs) $name]
	if { $idx < 0 } {
	    lappend Server(watchs) $name
	    if { [::param::alive $id] } {
		::permclient::write $id "WATCH $name"
		::permclient::write $id "GET $name"

		${log}::info "Waiting for value of $name"
		vwait ${parname}($name)
		return $Params($name)
	    } elseif { $block } {
		${log}::info "Waiting for value of $name"
		vwait ${parname}($name)
		return $Params($name)
	    } else {
		${log}::warn "Cannot get value for: $name"
		return -code error "Cannot get value for: $name"
	    }
	} elseif { $block } {
	    ${log}::info "Waiting for value of $name"
	    vwait ${parname}($name)
	    return $Params($name)
	} else {
	    ${log}::warn "No value yet for parameter: $name"
	    return -code error "No value yet for parameter: $name"
	}
    }

    return ""
}


# ::param::set -- Set a parameter at the server
#
#	Set a key, value pair at the server, this association will
#	last the life of the server only.
#
# Arguments:
#	id	Identifier of connection to server.
#	name	Name of remote parameter to get
#	value	Value of parameter
#
# Results:
#	Returns 1 on success, 0 otherwise
#
# Side Effects:
#	Actively talks to the server
proc ::param::set { id name value } {
    variable log

    if { [::param::alive $id] } {
	${log}::debug "Setting $name = $value"
	return [::permclient::write $id "SET $name $value"]
    }
    
    return 0
}


# ::param::store -- Store a parameter at the server
#
#	Store a key,value pair at the server.  This association will
#	be made permanently available.
#
# Arguments:
#	id	Identifier of connection to server.
#	name	Name of remote parameter to get
#	value	Value of parameter
#
# Results:
#	Returns 1 on success, 0 otherwise
#
# Side Effects:
#	Actively talks to the server
proc ::param::store { id name value } {
    variable log

    if { [::param::alive $id] } {
	${log}::debug "Storing $name = $value"
	return [::permclient::write $id "STORE $name $value"]
    }
    
    return 0
}


# ::param::close -- Close parameter server connection
#
#	Close connection and cleanup local cache.
#
# Arguments:
#	id	Identifier of connection to server.
#
# Results:
#	None
#
# Side Effects:
#	Tells the server that we leave.
proc ::param::close { id } {
    if { [::param::alive $id] } {
	::permclient::write $id "QUIT"
    }
    ::permclient::delete $id

    ::set varname "::param::Server"
    upvar \#0 $varname Server
    catch "unset Server"

    ::set varname "::param::Cache_$id"
    upvar \#0 $varname Params
    catch "unset Params"
}
