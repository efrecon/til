##################
## Module Name     --  prompt.tcl
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##    This program acts as a generic prompt towards a remote server.  Commands,
##    taken from the command line, are sent to the remote server.  Any answer
##    is printed on stdout.
##
##################
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.



# Array Name       --  PMPT
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Global state of this program.
#
# Contents:
#    arg1	-
#    arg2	-
array set PMPT {
    permid      ""
    proxy       ""
    finished    0
}

source [file join [file dirname $argv0] argutil.tcl]
::argutil::accesslib tcllib

# Now parse the options and put the result into the GE global state array
package require cmdline

set options {
    { verbose.arg "notice" "Verbosity Level" }
    { host.arg "localhost" "Host name of server to connect to" }
    { port.integer "3448" "Port number of server to connect to" }
    { proxy.arg "" "Proxy bridge to use for communication" }
    { period.integer "5" "Reconnection period when lost connection" }
    { java "" "Is this a prompt to a Java-UTF8 server?" }
}

set inited [::argutil::initargs PMPT $options]
if { [catch {cmdline::typedGetoptions argv $options} optlist] != 0 } {
    puts [cmdline::typedUsage $options "accepts the following options:"]
    exit
}
array set PMPT $optlist
::argutil::boolean PMPT java
foreach key $inited {
    ::argutil::makelist PMPT($key)
}

# Include modules that we depend on.  This is complicated to be able
# to address separately modules in the verbose specification.
::argutil::accesslib til
::argutil::loadmodules [list java permclient] $PMPT(verbose)
::argutil::fix_outlog


# Command Name     --  read_input
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Read input from the file descriptor passed as a parameter and send it
# further to the server to which we are connected.
#
# Arguments:
#    fd 	- File to read from
proc read_input { { fd stdin } } {
    global PMPT

    set line [gets $fd]
    if { $line != "" } {
	::permclient::write $PMPT(permid) "$line"
    }
}


# Command Name     --  out_data
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# This procedure is called back whenever som data coming from the server
# has been read.  It prints logs out its results.
#
# Arguments:
#    arg1	-
#    arg2	-
proc out_data { id line } {
    puts "$line"
}



# Arrange to be a permanent client of the remote server.
set PMPT(permid) [::permclient::new $PMPT(host) $PMPT(port) out_data \
		      -poll $PMPT(period) -proxy $PMPT(proxy)]
if { $PMPT(java) } {
    ::permclient::ioctl $PMPT(permid) \
	::java::utfsock_read ::java::utfsock_write ::java::utfsock_close
}

# Arrange to read from stdin as soon as something is available and the
# server is alive.
::permclient::waitalive $PMPT(permid)
fileevent stdin readable [list read_input stdin]


vwait $PMPT(finished)
