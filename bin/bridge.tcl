##################
## Program Name    --  bridge.tcl
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##    This program implements port bridging between clients (typically
##    behind a firewall) and servers that would not be accessible
##    otherwise.  It is written out of paranoia. I know that this will
##    happen! :-(
##
##################
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.


# Array Name       --  BRIDGE
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Global state of this program
#
# Contents:
#    servid	- Identifier of command server that we implement.
#    finish	- End indicator
#    autoconn	- Should we autoconnect to remote servers on first command?
array set BRIDGE {
    servid         -1
    finish         0
    autoconn       1
}


source [file join [file dirname $argv0] argutil.tcl]
argutil::accesslib tcllib

# Now parse the options and put the result into the PM global state array
package require cmdline

set options {
    { port.integer "80" "Port to listen to incoming client connections" }
    { verbose.arg "notice" "Verbosity Level" }
}

set inited [argutil::initargs BRIDGE $options]
if { [catch {cmdline::typedGetoptions argv $options} optlist] != 0 } {
    puts [cmdline::typedUsage $options "accepts the following options:"]
    exit
}
array set BRIDGE $optlist
foreach key $inited {
    argutil::makelist BRIDGE($key)
}

# Include modules that we depend on.  This is complicated to be able
# to address separately modules in the verbose specification.
argutil::accesslib til
argutil::loadmodules [list cmdserver java permclient] $BRIDGE(verbose)

# Initialise local logging facility
package require logger
set BRIDGE(log) [::logger::init bridge]
$BRIDGE(log)::setlevel $BRIDGE(verbose)

argutil::fix_outlog

# Command Name     --  incoming_remote
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# This command will be called back for each line coming from the remote
# server.  The line is forwarded to the effective bridged client.
#
# Arguments:
#    id 	- Identifier of permanent client.
#    line	- Line
proc incoming_remote { id line } {
    global BRIDGE

    foreach sock [::cmdserver::clients $BRIDGE(servid)] {
	set remotes [::cmdserver::assoc_get $sock "remotes"]
	set idx [lsearch $remotes $id]
	if { $idx >= 0 } {
	    set c_info [::permclient::info $id]
	    set host [lindex $c_info 1]
	    set port [lindex $c_info 2]
	    ::cmdserver::broadcast $BRIDGE(servid) "$host/$port $line" $sock
	}
    }
}



# Command Name     --  connect
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Connect to remote server and establish bridge.
#
# Arguments:
#    sock	- Socket of the client requesting the connection
#    host	- Host name of remote server.
#    port	- Port number at remote server.
proc connect { sock host port } {
    global BRIDGE

    set remotes [::cmdserver::assoc_get $sock "remotes"]
    set existing [::permclient::find $host $port]
    set found 0
    foreach id $existing {
	if { [lsearch $remotes $id] >= 0 } {
	    set found 1
	}
    }
    if { ! $found } {
	$BRIDGE(log)::notice "Connecting to $host:$port"
	set id [::permclient::new $host $port incoming_remote -block 100]
	lappend remotes $id
	::cmdserver::assoc_put $sock "remotes" $remotes
    }
}


# Command Name     --  disconnect
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Disconnect from remote server and remove bridge.
#
# Arguments:
#    sock	- Socket of the client requesting the disconnection
#    host	- Host name of remote server.
#    port	- Port number at remote server.
proc disconnect { sock host port } {
    global BRIDGE

    $BRIDGE(log)::notice "Disconnecting $host:$port"
    set remotes [::cmdserver::assoc_get $sock "remotes"]
    set existing [::permclient::find $host $port]
    foreach id $existing {
	set idx [lsearch $remotes $id]
	if { $idx >= 0 } {
	    set remotes [lreplace $remotes $idx $idx]
	    ::permclient::delete $id
	    ::cmdserver::assoc_put $sock "remotes" $remotes
	}
    }
}


# Command Name     --  incoming
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Procedure called back on incoming commands from clients.
#
# Arguments:
#    sock	- Socket connection to client
#    cmd	- Command issued
#    remaining	- Remaining arguments.
proc incoming { sock cmd remaining } {
    global BRIDGE

    switch $cmd {
	CONNECT {
	    set host [lindex $remaining 0]
	    set port [lindex $remaining 1]
	    if { $port == "" } {
		set split_host [split $host "/"]
		set host [lindex $split_host 0]
		set port [lindex $split_host 1]
	    }
	    connect $sock $host $port
	}
	DISCONNECT {
	    set host [lindex $remaining 0]
	    set port [lindex $remaining 1]
	    if { $port == "" } {
		set split_host [split $host "/"]
		set host [lindex $split_host 0]
		set port [lindex $split_host 1]
	    }
	    disconnect $sock $host $port
	}
	default {
	    set split_host [split $cmd "/"]
	    set host [lindex $split_host 0]
	    set port [lindex $split_host 1]
	    if { [llength $split_host] >= 2 && [string is integer $port] } {
		if { $BRIDGE(autoconn) } {
		    connect $sock $host $port
		}
		set remotes [::cmdserver::assoc_get $sock "remotes"]
		set existing [::permclient::find $host $port]
		set found 0
		foreach id $existing {
		    if { [lsearch $remotes $id] >= 0 } {
			::permclient::write $id $remaining
		    }
		}
	    } else {
		$BRIDGE(log)::warn "$cmd is an unrecognised command!"
	    }
	}
    }
}



# Start serving
set BRIDGE(servid) [::cmdserver::new $BRIDGE(port) \
			[list CONNECT DISCONNECT __DEFAULT__] \
			incoming "Bridge Server"]
if { $BRIDGE(servid) < 0 } {
    exit
}
::cmdserver::assocs $BRIDGE(servid) "remotes"

# Wait forever!
vwait $BRIDGE(finish)
