##################
## Program Name    --  params.tcl
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##   This program implements a server for parameter storage, query and watch.
##   The main idea behind this server is to allow a set of communicating
##   processes to share information in a seamless way. 
##
##################
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.


# One possible extension to bring to this program would consist in
# watching the various data files that it reads, so that it would
# re-read them and bring all remote client in sync. with the newly
# edited parameters.

# Another extension would be to remember which process has set a
# parameter and watch connection with that process.  The parameter
# would be automatically unset when the remote setting process exists.
# Perhaps are these two different SET commands: a temporary one and a
# permanent one.  Then, permanent variables should be saved to a file
# and read at the next restart, probably.


# Array Name       --  PM
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Global state of this program
#
# Contents:
#    clients	- List of client sockets.
#    socket	- Socket for server connection
#    finish	- End indicator
#    infile	- File to initiate with on start up.
array set PM {
    servid         -1
    finish         0
    storagedb      "persistent_params.prm"
    store          ""
}

source [file join [file dirname $argv0] argutil.tcl]
argutil::accesslib tcllib


# Array Name       --  PARAMS
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# This array will contain the parameters that are served by this program.
array set PARAMS {}


# Now parse the options and put the result into the PM global state array
package require cmdline

set options {
    { port.integer "3272" "Port to listen to incoming client connections" }
    { verbose.arg "warn" "Verbosity Level" }
    { infiles.arg "" "Files to read volatile initial state from" }
    { allow.arg "*" "List of hostname/ip allow patterns for incoming clients" }
    { deny.arg "" "List of hostname/ip deny patterns for incoming clients" }
}

set inited [argutil::initargs PM $options]
if { [catch {cmdline::typedGetoptions argv $options} optlist] != 0 } {
    puts [cmdline::typedUsage $options "accepts the following options:"]
    exit
}
array set PM $optlist
foreach key $inited {
    argutil::makelist PM($key)
}


# Include modules that we depend on.  This is complicated to be able
# to address separately modules in the verbose specification.
argutil::accesslib til
argutil::loadmodules [list cmdserver java diskutil param::store] $PM(verbose)

# Initialise local logging facility
package require logger
set PM(log) [::logger::init params]
$PM(log)::setlevel $PM(verbose)
argutil::fix_outlog


# Command Name     --  get_interested
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Get the sockets of the clients interested in a parameter modification.
# Return a possibly empty list of these sockets.
#
# Arguments:
#    param	- Parameter to poll for.
proc get_interested { param } {
    global PM

    set interest ""
    foreach sock [::cmdserver::clients $PM(servid)] {
	set watchs [::cmdserver::assoc_get $sock "watchs"]
	foreach ptn $watchs {
	    if { [string match $ptn $param] } {
		lappend interest $sock
	    }
	}
    }

    return [lsort -unique $interest]
}



# Command Name     --  watch_param
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Watch when a paramter is being set or unset.
#
# Arguments:
#    type	- Type (SET or UNSET)
#    store	- Identifier of store
#    param	- Parameter being concerned
proc watch_param { type store param } {
    global PM

    if { $type == "SET" } {
	set interest [get_interested $param]
	::cmdserver::broadcast $PM(servid) \
	    "SET $param [::param::store::get $store $param]" $interest
    } elseif { $type == "UNSET" } {
	set interest [get_interested $param]
	::cmdserver::broadcast $PM(servid) "UNSET $param" $interest
    }
}


# Command Name     --  add_watch
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Add a parameter watch so that a client will be notified whenever a 
# matching parameter gets modified.
#
# Arguments:
#    ptn	- Pattern to add as a watch
#    sock	- Socket connection to client.
proc add_watch { ptn sock } {
    global PM

    set watchs [::cmdserver::assoc_get $sock "watchs"]
    set idx [lsearch -exact $watchs $ptn]
    if { $idx < 0 } {
	$PM(log)::notice \
	    "Add watch on $ptn for [::cmdserver::client_name $sock]"
	lappend watchs $ptn
	::cmdserver::assoc_put $sock "watchs" $watchs
    }
}


# Command Name     --  remove_watch
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Remove a parameter watch that has been previously setup using add_watch.
# The pattern specified for removal must be identical to the one set up for
# addition.
#
# Arguments:
#    ptn	- Pattern to remove as a watch
#    sock	- Socket connection to client.
proc remove_watch { ptn sock } {
    global PM

    set watchs [::cmdserver::assoc_get $sock "watchs"]
    set idx [lsearch -exact $watchs $ptn]
    if { $idx >= 0 } {
	$PM(log)::notice \
	    "Removed watch on $ptn for [::cmdserver::client_name $sock]" 3
	set watchs [lreplace $watchs $idx $idx]
	::cmdserver::assoc_put $sock "watchs" $watchs
    }
}


# Command Name     --  incoming
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Procedure called back on incoming commands from clients.
#
# Arguments:
#    sock	- Socket connection with client.
proc incoming { sock cmd remaining } {
    global PM PARAMS

    switch $cmd {
	WATCH {
	    foreach w $remaining {
		add_watch $w $sock
	    }
	}
	IGNORE {
	    foreach w $remaining {
		remove_watch $w $sock
	    }
	}
	SET {
	    ::param::store::multiset $PM(store) $remaining
	}
	STORE {
	    ::param::store::multistore $PM(store) $remaining
	}
	UNSET {
	    ::param::store::unset $PM(store) $remaining
	}
	UNSTORE {
	    ::param::store::unstore $PM(store) $remaining
	}
	GET {
	    set answer [::param::store::multiget $PM(store) $remaining]
	    if { [llength $answer] > 0 } {
		::cmdserver::broadcast $PM(servid) "SET $answer" $sock
	    }
	    foreach ptn $remaining {
		if { [lsearch -glob $answer $remaining] < 0 } {
		    ::cmdserver::broadcast $PM(servid) "UNKNOWN $ptn" $sock
		}
	    }
	}
	LIST {
	    array set answer [::param::store::multiget $PM(store) $remaining]
	    ::cmdserver::broadcast $PM(servid) \
		"LIST [array names answer]" $sock
	}
    }
}





# Start serving
set PM(servid) [::cmdserver::new $PM(port) \
		    "WATCH IGNORE SET STORE UNSET UNSTORE GET LIST" \
		    incoming "Parameter Server"]
if { $PM(servid) < 0 } {
    exit
}
::cmdserver::assocs $PM(servid) "watchs"
::cmdserver::restrict $PM(servid) $PM(allow) $PM(deny)

set PM(store) [::param::store::new $PM(storagedb)]
foreach fname [::diskutil::fname_resolv $PM(infiles)] {
    ::param::store::read $PM(store) $fname
}
::param::store::addwatch $PM(store) * watch_param

vwait PM(finish)
