# validate.tcl --
#
#	This modules provides a facility for validating the HTTPD
#	server and making sure that we discover the proper hostname
#	that it runs on.
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

package require Tcl 8.2
package require logger

package require minihttpd

namespace eval ::minihttpd::validate {
    variable VALIDATE
    if {![::info exists VALIDATE]} {
	array set VALIDATE {
	    timeout    250
	}
    }
}

proc ::minihttpd::validate::init { port } {
    set varname "[namespace parent [namespace current]]::Server_${port}"
    upvar \#0 $varname Server

    # Self validate the server, i.e. do a request for / on
    # ourselves so that we trigger the hostname discovery
    # mechanism.  If we do not do this, we will have to wait until
    # the first request before ::minihttpd::fullurl returns a good
    # value.
    if { $Server(-selfvalidate) != "" } {
	::http::register $Server(protocol) $port $Server(socket_cmd)
	set Server(selfvalidation_urls) [list]
	if { $Server(-externhost) != "" } {
	    set Server(selfvalidation_urls) \
		[list "$Server(protocol)://$Server(-externhost):$port/"]
	}
	lappend Server(selfvalidation_urls) \
	    [[namespace parent [namespace current]]::fullurl $port] \
	    "$Server(protocol)://localhost:$port/"
	__self $port
    }
}



# ::minihttpd::__self -- Initiate one validation step
#
#	This command will initiate self validation of the host name of
#	the server through trying to fetch one of the (remaining) self
#	validation URLs.  It also finishes up the validation process
#	through defaulting back to some (hopefully) decent IP address
#	in the end.
#
# Arguments:
#	port	Port number of one of our HTTP servers.
#
# Results:
#	None.
#
# Side Effects:
#	Will log on disk if required
proc ::minihttpd::validate::__self { port } {
    variable VALIDATE
    namespace upvar [namespace parent] log log

    if { [[namespace parent [namespace current]]::listening $port] } {
	set varname "[namespace parent [namespace current]]::Server_${port}"
	upvar \#0 $varname Server
	
	if { [llength $Server(selfvalidation_urls)] == 0 } {
	    # No more validation URLs found, default to the IP address
	    # if we have a junk hostname and a decent IP address.
	    if { $Server(ip) != "" && $Server(ip) != "127.0.0.1" \
		    && ( $Server(hostname) == "" \
		    || $Server(hostname) == "localhost" )} {
		[namespace parent [namespace current]]::__log $port \
		    "Last resort: defaulting to IP address"
		set Server(hostname) $Server(ip)
	    }
	    [namespace parent [namespace current]]::__log $port \
		"Server's root is:\
                 [[namespace parent [namespace current]]::fullurl $port]"
	} else {
	    # Pick up the next valid validation URL and try to get it
	    # (from ourselves!).
	    foreach topurl $Server(selfvalidation_urls) {
		[namespace parent [namespace current]]::__log $port \
		    "Self validating through fetching $topurl"
		set cmd [list ::http::geturl $topurl \
			     -validate 1 \
			     -timeout $VALIDATE(timeout) \
			     -command [list [namespace current]::__root $port]]
		if { [catch {eval $cmd} err] } {
		    [namespace parent [namespace current]]::__log $port \
			"Error on validation: $err"
		} else {
		    break
		}
	    }
	}
    } else {
	${log}::warn "Not listening for HTTP connections on $port!"
    }
}


# ::minihttpd::__hostname -- Verify the host name
#
#	This command is called as a result of the validation check
#	once a host name has been discovered.  It attempts to get the
#	root of this web server through that very host name.  If that
#	fails, the next validation URL will be tried.
#
# Arguments:
#	port	Port number of one of our HTTP servers.
#	token	Token as returned by ::http::geturl
#
# Results:
#	Return 1 if server's true host name was discovered, 0 otherwise
#
# Side Effects:
#	None.
proc ::minihttpd::validate::__hostname { port token } {
    if { [[namespace parent [namespace current]]::listening $port] } {
	set varname "[namespace parent [namespace current]]::Server_${port}"
	upvar \#0 $varname Server

	if { [::http::status $token] != "ok" } {
	    set Server(hostname) ""
	    __self $port
	} elseif { [::http::status $token] == "ok" } {
	    if { $Server(hostname) == "localhost" \
		    || $Server(hostname) == "" } {
		__self $port
	    } else {
		[namespace parent [namespace current]]::__log $port \
		    "Server's root is:\
                     [[namespace parent [namespace current]]::fullurl $port]"
	    }
	}
    }
    ::http::cleanup $token
}


# ::minihttpd::validate::__root -- Validate the root
#
#	This command is called as a result of the initial fetch of the
#	root of the web server.  It prints an error message if we did
#	not manage to initialise the hostname correctly.
#
# Arguments:
#	port	Port number of one of our HTTP servers.
#	token	Token as returned by ::http::geturl
#
# Results:
#	Return 1 if server's true host name was discovered, 0 otherwise
#
# Side Effects:
#	None.
proc ::minihttpd::validate::__root { port token } {
    variable VALIDATE

    namespace upvar [namespace parent] log log
    set result 0
    if { [[namespace parent [namespace current]]::listening $port] } {
	set varname "[namespace parent [namespace current]]::Server_${port}"
	upvar \#0 $varname Server

	upvar \#0 $token htstate
	set Server(selfvalidation_urls) \
		[lrange $Server(selfvalidation_urls) 1 end]
	
	# Force host name to IP address if self-validation tells us so.
	if { $Server(ip) != "" \
		 && [string equal -nocase $Server(-selfvalidate)  "ip"] } {
	    set Server(hostname) $Server(ip)
	}
	
	if { $Server(hostname) != "" } {
	    ${log}::debug \
		    "Server host name discovered: $Server(hostname), trying it"
	    set cmd [list ::http::geturl \
			 [[namespace parent [namespace current]]::fullurl \
			      $port] \
			 -timeout $VALIDATE(timeout) \
			 -validate 1 \
			 -command [list [namespace current]::__hostname $port]]
	    if { [catch {eval $cmd} err] } {
		[namespace parent [namespace current]]::__log $port \
		    "Error on hostname verification: $err"
	    }
	    set result 1
	} else {
	    ${log}::warn "Self-validation of host name through\
		          $htstate(url) did not succeed."
	    __self $port
	}
    } else {
	${log}::warn "Not listening for HTTP connections on $port!"
    }

    ::http::cleanup $token

    return $result
}


package provide minihttpd::validate 1.0