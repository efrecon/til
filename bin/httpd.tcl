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


# Note the format of the authorization specification, either as a direct list on
# the command line or from a file (when the value of the option starts with a @)
# is a list which is a multiplier of 3 elements. When reading from a file, lines
# starting with a # mark will be ignored, and so will empty lines. To add an
# empty line, specify {}, "" or a - for the line content. The specification
# elements are, in order:
# A glob-style pattern matching the URL to apply the authorization on
# The realm for this authorization challenge
# A list of user:pass specifications (formatted with the colon in between)

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
    { authorization.arg "" "Multiple of three list or @ followed by path to file, empty to switch off: URL matching pattern, realm, list of user:pass" }
    { pki.arg "" "List of two (resolvable) paths to files: the public and private key for the server (will switch to HTTPS serving)"}
    { ciphers.arg "" "List of ciphers (e.g. tls1, ssl3, etc.) to support, empty for good default" }
    { hostname.arg "" "Which hostname the server is responding to (empty for good guess)" }
    { ranges.arg {0.0.0.0/0 ::/0} "List of IP ranges of client addresses allowed, in CIDR notation; or @ followed by path to file" }
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

# Convert command-line arguments to arguments that can be passed further to the
# server command (this is almost a 1:1 matching). This construct the command in
# the list variable cmd.
if { [string index $HTTPD(authorization) 0] eq "@" } {
    set fname [string trim [string range $HTTPD(authorization) 1 end]]
    set HTTPD(authorization) [::diskutil::lread $fname 3 \
				    "Authorization Specification"]
}
if { [string index $HTTPD(ranges) 0] eq "@" } {
    set fname [string trim [string range $HTTPD(ranges) 1 end]]
    set HTTPD(ranges) [::diskutil::lread $fname 1 "CIDR Ranges"]
}
set rootdir [::diskutil::fname_resolv $HTTPD(root)]
set logfile [::diskutil::fname_resolv $HTTPD(logfile)]
set cmd [list ::minihttpd::new $rootdir $HTTPD(port) \
			-logfile $logfile \
			-dirlist $HTTPD(allow) \
			-ranges $HTTPD(ranges)]
foreach {opt copt} [list authorization authorization pki pki ciphers ciphers hostname externhost] {
    if { $HTTPD($opt) ne "" } {
	lappend cmd -[string trimleft $copt -] $HTTPD($opt)
    }
}

# Start the server.
set HTTPD(port) [eval $cmd]
if { $HTTPD(port) < 0 } {
    exit
} else {
    puts "Web server started on port $HTTPD(port)"
}

vwait HTTPD(finished);   # Wait forever, we want an event loop!
