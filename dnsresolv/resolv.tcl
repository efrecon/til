# resolv.tcl - Copyright (c) 2002 Emmanuel Frecon <emmanuel@sics.se>
#
# Original Author --  Emmanuel Frecon - emmanuel@sics.se
# Modified by Pat Thoyts <patthoyts@users.sourceforge.net>
#
#  A super module on top of the dns module for host name resolution.
#  There are two services provided on top of the regular Tcl library:
#  Firstly, this module attempts to automatically discover the default
#  DNS server that is setup on the machine that it is run on.  This
#  server will be used in all further host resolutions.  Secondly, this
#  module offers a rudimentary cache.  The cache is rudimentary since it
#  has no expiration on host name resolutions, but this is probably
#  enough for short lived applications.
#
# -------------------------------------------------------------------------
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
# -------------------------------------------------------------------------
#
# $Id: resolv.tcl,v 1.2 2006/04/28 09:49:08 efrecon Exp $

package require dns 1.0;                # tcllib 1.3
package require logger;

package provide dnsresolv 1.2

namespace eval ::dnsresolv {
    variable version 1.2
    variable rcsid {$Id: resolv.tcl,v 1.2 2006/04/28 09:49:08 efrecon Exp $}

    namespace export resolve init ignore hostname inverse

    variable R
    if {![info exists R]} {
        array set R {
            initdone   0
            dns        ""
            dnsdefault ""
            ourhost    ""
            search     {}
	    loglevel   warn
        }
	variable log [::logger::init dnsresolv]
	${log}::setlevel $R(loglevel)
    }
}


# ::dnsresolv::loglevel -- Set/Get current log level.
#
#	Set and/or get the current log level for this library.
#
# Arguments:
#	loglvl	New loglevel
#
# Results:
#	Return the current log level
#
# Side Effects:
#	None.
proc ::dnsresolv::loglevel { { loglvl "" } } {
    variable R
    variable log

    if { $loglvl != "" } {
	if { [catch "${log}::setlevel $loglvl"] == 0 } {
	    set R(loglevel) $loglvl
	}
    }

    return $R(loglevel)
}


# ::dnsresolv::ignore -- Selectively remove cache entry
#
#	Remove a host name resolution from the cache, if present, so
#	that the next resolution will query the DNS server again.
#
# Arguments:
#	hostnm	Name of host to remove from the cache
#
# Results:
#	none
#
# Side Effects:
#	None.
proc ::dnsresolv::ignore { hostname } {
    variable Cache
    catch {unset Cache($hostname)}
    return
}


# ::dnsresolv::myIP -- Return own IP
#
#	Find out localhost's IP address
#
# Results:
#	Return the first IP address of the current host
#
# Side Effects:
#	Creates a socket server to guess the IP address
proc ::dnsresolv::myIP {} {
    set TheServer [socket -server none -myaddr [info hostname] 0]
    set MyIP [lindex [fconfigure $TheServer -sockname] 0]
    close $TheServer
    return $MyIP
}


# ::dnsresolv::init -- Initialise the library
#
#	Initialise this module with a known host name.  This host (not
#	mandatory) will become the default if the library was not able
#	to find a DNS server. This command can be called several
#	times, its effect is double: actively looking for the default
#	DNS server setup on the running machine; and emptying the host
#	name resolution cache.
#
# Arguments:
#	dftdns	Default DNS server to use
#	search	Search list of DNS servers
#
# Results:
#	Return the dns server that we should use
#
# Side Effects:
#	Uses the nslookup command to actively search
proc ::dnsresolv::init { {dftdns ""} {search {}}} {
    variable R
    variable Cache

    # Clean the resolver cache
    catch {unset Cache}

    # Record the default DNS server and search list.
    set R(dnsdefault) $dftdns
    set R(search) $search

    # Now do some intelligent lookup.  We do this on the current
    # hostname to get a chance to get back some (full) information on
    # ourselves.  A previous version was using 127.0.0.1, not sure
    # what is best.
    set res [catch [list exec nslookup [info hostname]] lkup]
    # We do not test on the value of "res" since nslookup could be
    # answering an error, while still providing some useful
    # information.  Instead, we look ahead for some string that hints
    # at some interesting pieces of information.
    if { [string first "SERVER:" [string toupper $lkup]] >= 0 } {
	set l [split $lkup]
	set nl ""
	foreach e $l {
	    if { [string length $e] > 0 } {
		lappend nl $e
	    }
	}

        # Now, a lot of mixture to arrange so that hostname points at the
        # DNS server that we should use for any further request.  This
        # code is complex, but was actually tested behind a firewall
        # during the SITI Winter Conference 2003.  There, strangly,
        # nslookup returned an error but a DNS server was actually setup
        # correctly...
        set hostname ""
	set len [llength $nl]
	for { set i 0 } { $i < $len } { incr i } {
	    set e [lindex $nl $i]
	    if { [string match -nocase "*server:*" $e] } {
		set hostname [lindex $nl [expr {$i + 1}]]
                if { [string match -nocase "UnKnown" $hostname] } {
                    set hostname ""
                }
		break
	    }
	}

	if { $hostname != "" } {
	    set R(dns) $hostname
	} else {
            for { set i 0 } { $i < $len } { incr i } {
                set e [lindex $nl $i]
                if { [string match -nocase "*address:*" $e] } {
                    set hostname [lindex $nl [expr {$i + 1}]]
                    break
                }
            }
            if { $hostname != "" } {
                set R(dns) $hostname
            }
	}
    }

    if {$R(dns) == ""} {
        set R(dns) $R(dnsdefault)
    }


    # Start again to find our full name
    set ourhost ""
    if {$res == 0} {
        set dot [string first "." [info hostname]]
        if { $dot < 0 } {
            for { set i 0 } { $i < $len } { incr i } {
                set e [lindex $nl $i]
		if { $e == "***" } {
		    for { set j 0 } { $j < $len } { incr j } {
			if { [lindex $nl $j] == "" } {
			    break
			}
		    }
		    set i $j
                } elseif { [string match -nocase "*name:*" $e] } {
                    set ourhost [lindex $nl [expr {$i + 1}]]
                    break
                }
            }
            if { $ourhost == "" } {
                if { ! [regexp {\d+\.\d+\.\d+\.\d+} $hostname] } {
                    set dot [string first "." $hostname]
                    set ourhost [format "%s%s" [info hostname] \
                                     [string range $hostname $dot end]]
                } else {
		    set ourhost [myIP]
		}
            }
        } else {
            set ourhost [info hostname]
        }
    }
    
    if {$ourhost == ""} {
        set R(ourhost) [info hostname]
    } else {
        set R(ourhost) $ourhost
    }


    set R(initdone) 1

    return $R(dns)
}


# ::dnsresolv::resolve -- Resolve a host name
#
#	Resolve a host name to an IP address.  This is a wrapping
#	procedure around the basic services of the dns library.
#
#
# Arguments:
#	hstnm	Hostname to resolve to an IP address
#
# Results:
#	Return the IP of the host
#
# Side Effects:
#	Will talk to one or several DNS servers
proc ::dnsresolv::resolve { hstnm } {
    variable R
    variable Cache

    # Initialise if not already done. Auto initialisation cannot take
    # any known DNS server (known to the caller)
    if { ! $R(initdone) } { init }

    # Check whether this is not simply a raw IP address. What about
    # IPv6 ??
    # - We don't have sockets in Tcl for IPv6 protocols - [PT]
    #
    if { [regexp {\d+\.\d+\.\d+\.\d+} $hstnm] } {
	return $hstnm
    }

    # Look for hostname in the cache, if found return.
    if { [array names ::resolv::Cache $hstnm] != "" } {
	return $::resolv::Cache($hstnm)
    }

    # Scream if we don't have any DNS server setup, since we cannot do
    # anything in that case.
    if { $R(dns) == "" } {
	return -code error "No dns server provided"
    }

    set R(retries) 0
    set ip [Resolve $hstnm]

    # And store the result of resolution in our cache for further use.
    set Cache($hstnm) $ip

    return $ip
}


# ::dnsresolv::Resolve -- multi resolve using search list
#
#	Attempt to resolve hostname via DNS. If the name cannot be
#	resolved then iterate through the search list appending each
#	domain in turn until we get one that succeeds.
#
#
# Arguments:
#	hstnm	Hostname to resolve to an IP address
#
# Results:
#	Return the IP of the host
#
# Side Effects:
#	Will talk to one or several DNS servers
proc ::dnsresolv::Resolve {hstnm} {
    variable R
    set t [::dns::resolve $hstnm -server $R(dns)]
    ::dns::wait $t;                       # wait with event processing
    set status [dns::status $t]
    if {$status == "ok"} {
        set ip [lindex [::dns::address $t] 0]
        ::dns::cleanup $t
    } elseif {$status == "error"
              && [::dns::errorcode $t] == 3 
              && $R(retries) < [llength $R(search)]} {
        ::dns::cleanup $t
        set suffix [lindex $R(search) $R(retries)]
        incr R(retries)
        set new [lindex [split $hstnm .] 0].[string trim $suffix .]
        set ip [Resolve $new]
    } else {
        set err [dns::error $t]
        ::dns::cleanup $t
        return -code error "dns error: $err"
    }
    return $ip
}


# ::dnsresolv::inverse -- Inverse resolution of an IP to an address
#
#	Resolve an IP address back to its first host name.  This is a
#	wrapping procedure around the basic services of the dns
#	library.
#
#
# Arguments:
#	ip	IP address to inverse back
#
# Results:
#	Return the host name corresponding to the IP or an empty
#	string.
#
# Side Effects:
#	Will talk to one or several DNS servers
proc ::dnsresolv::inverse { ip } {
    variable R
    variable Cache
    variable log
    
    # Initialise if not already done. Auto initialisation cannot take
    # any known DNS server (known to the caller)
    if { ! $R(initdone) } {
	init
    }

    # Look for hostname in the cache, if found return.
    if { [array names Cache $ip] != "" } {
	return $Cache($ip)
    }

    # Scream if we don't have any DNS server setup, since we cannot do
    # anything in that case.
    if { $R(dns) == "" } {
	return -code error "No dns server provided"
    }

    # Now resolve in a clean way
    set names ""
    if { [catch {::dns::resolve $ip -server $R(dns)} t]} {
	${log}::warn "Cannot resolve $ip at $R(dns): $t"
	return ""
    }
    if { [catch {::dns::wait $t} err] } {
	${log}::warn "Error when waiting for DNS resolution completion: $err"
	return ""
    }
    if { [catch {::dns::name $t} dnsnames] } {
	${log}::warn "Error when getting names of $ip: $dnsnames"
	catch {::dns::cleanup $t}
	return ""
    }
    foreach n $dnsnames {
	if { [regexp {([\w\-]+\.)+([\w\-]{2,3})} $n] } {
	    lappend names $n
	}
    }
    set hostname [lindex $names 0]
    ::dns::cleanup $t

    # And store the result of resolution in our cache for further use.
    if { $hostname != "" } {
	set Cache($ip) $hostname
    }

    return $hostname
}


# ::dnsresolv::hostname -- Fully-qualified hostname
#
#	This is a wrapper around the [info hostname] command so that
#	it is will returned a fully qualified host name, with the
#	domain.  This is cached.
#
# Results:
#	Return our fully qualified hostname
#
# Side Effects:
#	None
proc ::dnsresolv::hostname {} {
    variable R

    # Initialise if not already done. Auto initialisation cannot take
    # any known DNS server (known to the caller)
    if { ! $R(initdone) } {
	init
    }

    return $R(ourhost)
}
