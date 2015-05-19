##################
## Program Name    --  multiplex.tcl
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##    This program multiplexes any command coming from any client to all
##    other connected clients.
##
##################
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.


# Array Name       --  MX
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Global state of this program
#
# Contents:
#    servid	- Identifier of command server that we implement.
#    finish	- End indicator
array set MX {
    servid         -1
    finish         0
}

source [file join [file dirname $argv0] argutil.tcl]
argutil::accesslib tcllib

# Now parse the options and put the result into the GE global state array
package require cmdline

set options {
    { port.integer "3744" "Port to listen to incoming client connections" }
    { verbose.arg "warn" "Verbosity Level" }
    { liveness.integer "-1" "Number of seconds to live, negative for infinity" }
    { back "" "Should we echo back to the sending client?" }
    { allow.arg "*" "List of hostname/ip allow patterns for incoming clients" }
    { deny.arg "" "List of hostname/ip deny patterns for incoming clients" }
}

set inited [argutil::initargs MX $options]
if { [catch {cmdline::typedGetoptions argv $options} optlist] != 0 } {
    puts [cmdline::typedUsage $options "accepts the following options:"]
    exit
}
array set MX $optlist
argutil::boolean MX back
foreach key $inited {
    argutil::makelist MX($key)
}

# Include modules that we depend on.  This is complicated to be able
# to address separately modules in the verbose specification.
#argutil::accesslib eflib
argutil::accesslib til
argutil::loadmodules [list cmdserver] $MX(verbose)
argutil::fix_outlog



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
    global MX

    switch $cmd {
	default {
	    set fwd [::cmdserver::clients $MX(servid)]
	    if { ! $MX(back) } {
		set idx [lsearch $fwd $sock]
		if { $idx >= 0 } {
		    set fwd [lreplace $fwd $idx $idx]
		}
	    }
	    if { [llength $fwd] > 0 } {
		::cmdserver::broadcast $MX(servid) "$cmd $remaining" $fwd
	    }
	}
    }
}



# Start serving
set MX(servid) \
    [::cmdserver::new $MX(port) "__DEFAULT__" incoming "Multiplexer"]
if { $MX(servid) < 0 } {
    exit
}
::cmdserver::restrict $MX(servid) $MX(allow) $MX(deny)

if { $MX(liveness) >= 0 } {
    after [expr $MX(liveness) * 1000] exit
}

# Wait forever!
vwait $MX(finish)
