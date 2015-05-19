##################
## Module Name     --  httpd.tcl
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##    This program implements a simple web server supporting the HEAD
##    and GET operations and with some initial support for POST
##    operations.
##
##################
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.


# Array Name       --  HTTPD
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Global state of this program.
#
# Contents:
#    arg1	-
#    arg2	-
array set HTTPD {
    finished    0
    logfile     ""
}

source [file join [file dirname $argv0] argutil.tcl]
::argutil::accesslib tcllib

# Now parse the options and put the result into the global state array
package require cmdline

set options {
    { verbose.arg "warn" "Verbosity Level" }
    { port.integer "-1" "Port number of server, negative means pick one" }
    { root.arg "%progdir%" "Root directory of web server" }
    { logfile.arg "" "Path to log file" }
    { allow.arg "*" "Directories allowed for listing" }
}

set inited [::argutil::initargs HTTPD $options]
if { [catch {cmdline::typedGetoptions argv $options} optlist] != 0 } {
    puts [cmdline::typedUsage $options "accepts the following options:"]
    exit
}
array set HTTPD $optlist
foreach key $inited {
    ::argutil::makelist HTTPD($key)
}

# Include modules that we depend on.  This is complicated to be able
# to address separately modules in the verbose specification.
::argutil::accesslib til
::argutil::loadmodules [list diskutil minihttpd] $HTTPD(verbose)
::argutil::fix_outlog

set rootdir [::diskutil::fname_resolv $HTTPD(root)]
set logfile [::diskutil::fname_resolv $HTTPD(logfile)]
set HTTPD(port) [::minihttpd::new $rootdir $HTTPD(port) \
		     -logfile $logfile -dirlist $HTTPD(allow)]
if { $HTTPD(port) < 0 } {
    exit
} else {
    puts "Web server started on port $HTTPD(port)"
}

vwait HTTPD(finished)
