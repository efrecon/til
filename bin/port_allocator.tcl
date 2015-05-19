##################
## Module Name     --  port_allocator.tcl
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##    This program acts as a port allocator service.  It associate
##    services to a ports and arranges to avoid that two different
##    services share the same port.  Port allocation is deterministic.
##    The program can either act as a one-shot port allocation 
##    service (through specifying a name for the service) or as a
##    port allocation server answering commands.
##
##################
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.



# Array Name       --  PA
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Global state of this program.
#
# Contents:
#    arg1	-
#    arg2	-
array set PA {
    servid      ""
    finished    0
    dns         "ns.sics.se"
}

source [file join [file dirname $argv0] argutil.tcl]
argutil::accesslib tcllib

# Now parse the options and put the result into the GE global state array
package require cmdline

set options {
    { verbose.arg "warn" "Verbosity Level" }
    { port.integer "4293" "Port number to listen to for incoming connections" }
    { allow.arg "*" "List of hostname/ip allow patterns for incoming clients" }
    { deny.arg "" "List of hostname/ip deny patterns for incoming clients" }
    { portsdb.arg "%progdir%/ports.txt" "Path to ports database" }
    { servicename.arg "" "Name of new service to create (empty for server)" }
    { servicedesc.arg "" "Description of service" }
}

set inited [argutil::initargs PA $options]
if { [catch {cmdline::typedGetoptions argv $options} optlist] != 0 } {
    puts [cmdline::typedUsage $options "accepts the following options:"]
    exit
}
array set PA $optlist
foreach key $inited {
    argutil::makelist PA($key)
}


# Include modules that we depend on.  This is complicated to be able
# to address separately modules in the verbose specification.
argutil::accesslib til
argutil::accesslib tcludp
argutil::loadmodules [list cmdserver portsalloc dnsresolv diskutil] $PA(verbose)

# Initialise local logging facility
package require logger
set PA(log) [::logger::init port_allocator]
$PA(log)::setlevel $PA(verbose)


argutil::fix_outlog


proc incoming { sock cmd remaining } {
    global PA

    switch $cmd {
	PORTINFO {
	    if { $remaining == "" } {
		set remaining [::portsalloc::listall]
	    }

	    foreach port $remaining {
		set pdesc [::portsalloc::find_byport $port]
		if { [llength $pdesc] > 0 } {
		    ::cmdserver::broadcast $PA(servid) \
			"PORT $port [lindex $pdesc 0] \"[lindex $pdesc 1]\"" \
			$sock
		}
	    }
	}
	ALLOCATE {
	    foreach {name desc} $remaining {
		set port [::portsalloc::add $name $desc]
		::cmdserver::broadcast $PA(servid) \
		    "ALLOCATED $name $port" $sock
	    }
	}
    }
}



# Guess the DNS server that can be used for address resolution on this
# very machine.  The code below should work on all platforms, since
# they all support nslookup (except MacOS <= 9).  If we do not manage,
# default to something here at SICS!
$PA(log)::notice "Guessing DNS server address..."
set dns ""
set res [catch "::dnsresolv::init $PA(dns)" dns]
if { $res == 0 && $dns != "" } {
    $PA(log)::notice "Found as $dns"
} else {
    $PA(log)::warn "DNS server not found, will have a hard time running!..."
}

::portsalloc::init [::diskutil::fname_resolv $PA(portsdb)]

if { $PA(servicename) == "" && $PA(servicedesc) == "" } {
    set PA(servid) [::cmdserver::new $PA(port) [list PORTINFO ALLOCATE] \
			incoming "Port Allocator"]
    ::cmdserver::restrict $PA(servid) $PA(allow) $PA(deny)
    vwait $PA(finished)
} else {
    set port [::portsalloc::add $PA(servicename) $PA(servicedesc)]
}
