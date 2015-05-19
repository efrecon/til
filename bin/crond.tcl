##################
## Module Name     --  cron.tcl
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##    This program implements a user-level cron daemon.  It offers a socket
##    interface to add/remove/list currently running entries.
##
##################
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.


# Array Name       --  CRON
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Global state of this program.
#
# Contents:
#    arg1	-
#    arg2	-
array set CRON {
    finished    0
}

source [file join [file dirname $argv0] argutil.tcl]
argutil::accesslib tcllib

# Now parse the options and put the result into the global state array
package require cmdline

set options {
    { verbose.arg "warn" "Verbosity Level" }
    { port.integer "3386" "Port number of server we implement" }
    { java "" "Is this a Java-UTF8 server?" }
    { crontab.arg "%progdir%/crontab.dta" "List of crontab files" }
    { allow.arg "*" "List of hostname/ip allow patterns for incoming clients" }
    { deny.arg "" "List of hostname/ip deny patterns for incoming clients" }
}

set inited [argutil::initargs CRON $options]
if { [catch {cmdline::typedGetoptions argv $options} optlist] != 0 } {
    puts [cmdline::typedUsage $options "accepts the following options:"]
    exit
}
array set CRON $optlist
argutil::boolean CRON java
foreach key $inited {
    argutil::makelist CRON($key)
}

# Include modules that we depend on.  This is complicated to be able
# to address separately modules in the verbose specification.
argutil::accesslib til
argutil::loadmodules [list cmdserver java cron crontab diskutil] $CRON(verbose)
argutil::fix_outlog



# Command Name     --  cron_incoming
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Procedure called back on incoming commands from clients.
#
# Arguments:
#    sock	- Socket connection with client.
proc cron_incoming { sock cmd remaining } {
    global CRON

    switch $cmd {
	LIST {
	    set all ""
	    set current [::cron::info]
	    foreach nfo $current {
		lappend all [lindex $nfo 0]
	    }
	    ::cmdserver::broadcast $CRON(s_id) "LIST $all" $sock
	}
	INFO {
	    foreach id $remaining {
		::cmdserver::broadcast $CRON(s_id) \
			"INFO [concat [::cron::info $id]]" $sock
	    }
	}
	DELETE {
	    foreach id $remaining {
		::cron::delete $id
	    }
	}
	ADD {
	    set cronid [::crontab::addline $remaining]
	    ::cmdserver::broadcast $CRON(s_id) "ADD $cronid" $sock
	}
	CHECKONCE {
	    set date [lindex $remaining 0]
	    set entries [lrange $remaining 1 end]
	    ::cron::checkonce $date $entries
	}
	RELOAD {
	    foreach nfo [::cron::info] {
		set id [lindex $nfo 0]
		::cron::delete $id
	    }
	    foreach fname [::diskutil::fname_resolv $CRON(crontab)] {
		::crontab::read $fname
	    }
	}
    }
}



# Command Name     --  cron_new_client
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Command called for each connecting new client
#
# Arguments:
#    Decided by the cmd_server interface.
proc cron_new_client { servid sock ip port } {
    global CRON

    if { $CRON(java) } {
	::java::utfsock_init $sock
	::cmdserver::client_ioctl $sock \
	    ::java::utfsock_read ::java::utfsock_write ::java::utfsock_close
    }
}


foreach fname [::diskutil::fname_resolv $CRON(crontab)] {
    ::crontab::read $fname
}
set CRON(s_id) \
    [::cmdserver::new $CRON(port) \
	 "LIST INFO DELETE ADD CHECKONCE RELOAD" \
	 cron_incoming "Cron Server"]
if { $CRON(s_id) < 0 } {
    exit
}
::cmdserver::restrict $CRON(s_id) $CRON(allow) $CRON(deny)
::cmdserver::connections_cb $CRON(s_id) cron_new_client ""

vwait CRON(finished)
