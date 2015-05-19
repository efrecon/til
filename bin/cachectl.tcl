##################
## Module Name     --  cachectl.tcl
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##    This program implements a cache creation and controller.  
##
##################
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.


# Array Name       --  CC
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Global state of this program.
#
# Contents:
#    arg1	-
#    arg2	-
array set CC {
    dns         "ns.sics.se"
    finished    0
}

source [file join [file dirname $argv0] argutil.tcl]
argutil::accesslib tcllib

# Now parse the options and put the result into the global state array
package require cmdline

set options {
    { verbose.arg "warn" "Verbosity Level" }
    { port.integer "3282" "Port number of server we implement" }
    { java "" "Is this a Java-UTF8 server?" }
    { fullpath "" "Fullpath of local cached files in answers?" }
    { cachesize.integer "2048" "Cache size, in MB" }
    { relax.arg "audio/* image/*" "Relax document types" }
    { allow.arg "*" "List of hostname/ip allow patterns for incoming clients" }
    { deny.arg "" "List of hostname/ip deny patterns for incoming clients" }
}

set inited [argutil::initargs CC $options]
if { [catch {cmdline::typedGetoptions argv $options} optlist] != 0 } {
    puts [cmdline::typedUsage $options "accepts the following options:"]
    exit
}
array set CC $optlist
argutil::boolean CC java
argutil::boolean CC fullpath
foreach key $inited {
    argutil::makelist CC($key)
}

# Include modules that we depend on.  This is complicated to be able
# to address separately modules in the verbose specification.
argutil::accesslib til
argutil::accesslib tcludp
argutil::loadmodules [list cmdserver java urlcache diskutil \
			 dnsresolv] $CC(verbose)

# Initialise local logging facility
package require logger
set CC(log) [::logger::init cachectl]
$CC(log)::setlevel $CC(verbose)
argutil::fix_outlog



# Command Name     --  quoteif
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Returns the original string surrounded by quotes only if it contains
# spaces.
#
# Arguments:
#    str	- String to possibly place inbetween quotes.
proc quoteif { str } {
    if { [string first " " $str] >= 0 } {
	return "\"$str\""
    } else {
	return "$str"
    }
}


# Command Name     --  cc_done
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Procedure called back when a URL has been fetched or when an error
# has occured for that URL.
#
# Arguments:
#    sock	- Socket of client to which to send file information
#    url	- URL requested
#    id 	- Identifier of cache
#    url	- URL that was got or on which we had an error
#    status	- Success status
#    fname	- Full path of file or error message.
proc cc_done { sock url id gurl status { fname "" } } {
    global CC

    if { $status == "OK" } {
	set retcode "GOT"
    } else {
	set retcode "ERROR"
    }

    set msg "$retcode $id [quoteif $url]"

    if { $status == "OK" } {
	if { $CC(fullpath) } {
	    append msg " [quoteif $fname]"
	} else {
	    append msg " [quoteif [file tail $fname]]"
	}
    } else {
	append msg " \"$fname\""
    }

    ::cmdserver::broadcast $CC(s_id) $msg $sock
}


# Command Name     --  cc_incoming
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Procedure called back on incoming commands from clients.
#
# Arguments:
#    sock	- Socket connection with client.
proc cc_incoming { sock cmd remaining } {
    global CC

    switch $cmd {
	CACHE {
	    set dir [lindex $remaining 0]
	    set rdir [::diskutil::fname_resolv $dir]
	    set id [::urlcache::new $rdir [expr {$CC(cachesize) * 1024}]]
	    foreach rax $CC(relax) {
		::urlcache::relax $id $rax
	    }
	    ::cmdserver::broadcast $CC(s_id) "CACHE $dir $id" $sock
	}
	GET {
	    set id [lindex $remaining 0]
	    foreach url [lrange $remaining 1 end] {
		if { [catch {::urlcache::open $id $url \
				 [list cc_done $sock $url]} cid] } {
		    ::cmdserver::broadcast $CC(s_id) \
			    "ERROR $id [quoteif $url]" $sock
		}
	    }
	}
	INFO {
	    set id [lindex $remaining 0]
	    if { [catch {::urlcache::cacheinfo $id content} content] == 0 } {
		# Gather all URLs matching all patterns
		set urls ""
		foreach ptn [lrange $remaining 1 end] {
		    foreach url $content {
			if { [string match $ptn $url] } {
			    lappend urls $url
			}
		    }
		}
		# Send back information for each of them, only once.
		foreach url [lsort -unique $urls] {
		    set uinfo [::urlcache::urlinfo $id $url]
		    ::cmdserver::broadcast $CC(s_id) \
			"INFO $id [quoteif $url] $uinfo" $sock
		}
	    }
	}
    }
}



# Command Name     --  cc_new_client
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Command called for each connecting new client
#
# Arguments:
#    Decided by the cmd_server interface.
proc cc_new_client { servid sock ip port } {
    global CC

    if { $CC(java) } {
	::java::utfsock_init $sock
	::cmdserver::client_ioctl $sock \
	    ::java::utfsock_read ::java::utfsock_write ::java::utfsock_close
    }
}

$CC(log)::notice "Guessing DNS server address..."
set dns ""
set res [catch "::dnsresolv::init $CC(dns)" dns]
if { $res == 0 && $dns != "" } {
    $CC(log)::info "Found as $dns"
} else {
    $CC(log)::info "Not found, will have a hard time running!..."
}

set CC(s_id) \
    [::cmdserver::new $CC(port) "CACHE GET INFO" \
	 cc_incoming "Cache Control Server"]
if { $CC(s_id) < 0 } {
    exit
}
::cmdserver::restrict $CC(s_id) $CC(allow) $CC(deny)
::cmdserver::connections_cb $CC(s_id) cc_new_client ""

vwait CC(finished)
