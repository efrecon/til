# massgeturl.tcl --
#
#	This modules provides a way to control and get a massive
#	number of URLs concurrently.
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.


# XXX: Maybe should we be intelligent about servers and timeouts.
# Increasing the timeout successively until a given limit, and storing
# this per-information server so that both the prefetching and the
# fetching share this information.


package require Tcl 8.2
package require logger
package require http 2.2
package require uri
package require htauth
package require uid
package require uobj

package provide massgeturl 1.0

namespace eval ::massgeturl {
    # Initialise the global state
    variable MGU
    if {![::info exists MGU]} {
	array set MGU {
	    idgene        0
	    loglevel      "warn"
	    connections   ""
	    conns         0
	    hosts         ""
	    tls_inited    0
	    -timeout      -1
	    -retries      3
	    -redirects    10
	    -maxhostconns 3
	    -maxconns     5
	    -progress     ""
	    -pgessfreq    10000
	    -priority     5
	    -maxinactivity 60000
	    -preparecmd   ""
	    enc_charmap   {}
	    dec_charmap   {}
	    qcheck_id     ""
	    qcheck_pulse  500
	    checkhint     0
	    inheritance   "-timeout -retries -redirects -progress -priority -maxinactivity -preparecmd"
	}
	set MGU(uids) [::uid::new]
	variable libdir [file dirname [file normalize [info script]]]
	::uobj::install_log massgeturl MGU
	::uobj::install_defaults massgeturl MGU
    }

    namespace export \
	loglevel new get head syncget synchead infile cancel defaults
}


# ::massgeturl::__checkinactivity -- Check download inactivity
#
#	This command is called back when the downloading operation has
#	been inactive for too long a period.
#
# Arguments:
#	cid	Identifier of get operation, as returned by ::get or ::head
#
# Results:
#	None.
#
# Side Effects:
#	Cancel the current operation.
proc ::massgeturl::__checkinactivity { cid } {
    variable MGU
    variable log

    set idx [lsearch $MGU(connections) $cid]
    if { $idx >= 0 } {
	set varname "::massgeturl::Fetch_${cid}"
	upvar \#0 $varname Fetch

	set period [expr [clock clicks -milliseconds] - $Fetch(lastactive)]
	${log}::info "$Fetch(url) download inactive for $period ms, aborting"
	set Fetch(activitycheck) ""
	::http::reset $Fetch(httoken) "max inactivity"
    }
}


# ::massgeturl::__tellprogress -- Call possible progress callback
#
#	This command calls back the progress callback if there is any.
#
# Arguments:
#	cid	Identifier of get operation, as returned by ::get or ::head
#	url	URL being worked on.
#	current	How many bytes currently got?
#	total	Total number of bytes to get (-1 if unknown).
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::massgeturl::__tellprogress { cid url current total } {
    variable MGU
    variable log

    set idx [lsearch $MGU(connections) $cid]
    if { $idx >= 0 } {
	set varname "::massgeturl::Fetch_${cid}"
	upvar \#0 $varname Fetch
	
	if { $Fetch(-progress) != "" } {
	    if { [catch {eval $Fetch(-progress) $cid {$Fetch(url)} {$url} \
			     $current $total} err] } {
		${log}::warn "Error when calling progress callback: $err"
	    }
	}
    }
}


# ::massgeturl::__progress -- Debug URL fetch progress
#
#	This command prints out fetching progress in the log in debug
#	mode.  Out is done on a regular basis only.
#
# Arguments:
#	cid	Identifier of get operation, as returned by ::get or ::head
#	token	http::geturl token
#	total	Total number of bytes to get
#	current	How many bytes currently got?
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::massgeturl::__progress { cid token total current } {
    variable MGU
    variable log

    set idx [lsearch $MGU(connections) $cid]
    if { $idx >= 0 } {
	set varname "::massgeturl::Fetch_${cid}"
	upvar \#0 $varname Fetch
	
	set now [clock clicks -milliseconds]

	# Reinitialise inactivity timer
	if { $Fetch(-maxinactivity) > 0 } {
	    set Fetch(lastactive) $now
	    after cancel $Fetch(activitycheck)
	    set Fetch(activitycheck) \
		[after $Fetch(-maxinactivity) \
		     ::massgeturl::__checkinactivity $cid]
	}

	# Check since how long we delivered a progress callback and do
	# it if necessary.
	if { [array names Fetch dn_lastout] == "" } {
	    set Fetch(dn_lastout) $now
	}
	
	set elapsed [expr {$now - $Fetch(dn_lastout)}]
	upvar \#0 $token state
	if { [llength [info vars $token]] > 0 } {
	    set url $state(url)
	} else {
	    ${log}::warn "Could not access URL information via standard\
                          http package!"
	    set url "<unknown:url>"
	}
	if { $MGU(-pgessfreq) >= 0 && $elapsed >= $MGU(-pgessfreq) } {
	    set Fetch(dn_lastout) $now
	    ${log}::debug \
		    "$url progress: $current bytes / $total bytes"
	}
	__tellprogress $cid $url $current $total
    }
}


proc ::massgeturl::__checkqueues { { hint "" } } {
    variable MGU
    variable log

    if { $hint eq "" } {
	return [string is true $MGU(checkhint)]
    }

    set MGU(checkhint) [string is true $hint]
    if { $MGU(checkhint) } {
	${log}::debug "Forcing queue check at a later time"
    } else {
	${log}::debug "Hinting not to check queues"
    }

    return $MGU(checkhint)
}


# ::massgeturl::__done_finalize -- Deliver callbacks
#
#	This command finalises a get operation.  It delivers the
#	callbacks and remove the connection from all its queues and
#	forget everything about it.
#
# Arguments:
#	cid	Identifier of get operation, as returned by ::get or ::head
#	status	Result of the operation ERROR or OK
#	token	http::geturl token
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::massgeturl::__done_finalize { cid status token } {
    variable MGU
    variable log

    #__checkqueues on
    set varname "::massgeturl::Fetch_${cid}"
    if { [::info exists $varname] } {
	# Get to information about the fetch
	upvar \#0 $varname Fetch

	# Remove local file if not OK
	if { $status != "OK" && $Fetch(fname) != "" } {
	    if { [catch {file delete $Fetch(fname)} err] } {
		${log}::warn "Could not remove $Fetch(fname): $err"
	    }
	}
	
	# Deliver all callbacks.
	foreach cb $Fetch(callbacks) {
	    if { [catch {eval $cb $Fetch(id) {$Fetch(url)} \
			     $status {$token}} err] } {
		${log}::warn \
		    "Error when invoking callback on $Fetch(url): $err"
	    }
	}
	
	# Get to information about its host connections
	set connid [::uid::id $MGU(uids) $Fetch(host)]
	set varname "::massgeturl::HostConn_${connid}"
	upvar \#0 $varname HostConn
	
	# Remove the fetch from the queue of pending connections for its host.
	set cidx [lsearch $HostConn(queue) $cid]
	if { $cidx >= 0 } {
	    set HostConn(queue) [lreplace $HostConn(queue) $cidx $cidx]
	}

	# Remove the fetch from the list of known connections and
	# forget the content of the variable.
	set idx [lsearch $MGU(connections) $cid]
	set MGU(connections) [lreplace $MGU(connections) $idx $idx]
	unset Fetch
    } else {
	${log}::warn "Fetch $cid is not registered anymore!"
    }
}


# ::massgeturl::__done -- URL get operation done
#
#	This command is called back when a get operation has finished.
#	It analyses the result and takes decision.  Redirects will be
#	followed, timeouts will lead to retries, etc.  Final success
#	or failure will be called back to the original caller.
#
# Arguments:
#	cid	Identifier of get operation, as returned by ::get or ::head
#	token	http::geturl token
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::massgeturl::__done { cid token } {
    variable MGU
    variable log

    upvar \#0 $token urlstate

    #__checkqueues on
    set keeptoken 0
    set idx [lsearch $MGU(connections) $cid]
    if { $idx >= 0 } {
	# Get to information about the fetch
	set varname "::massgeturl::Fetch_${cid}"
	upvar \#0 $varname Fetch

	# Get to information about it host connections
	set connid [::uid::id $MGU(uids) $Fetch(host)]
	set varname "::massgeturl::HostConn_${connid}"
	upvar \#0 $varname HostConn
	
	# Remove the fetch from the queue of pending connections for
	# its host.  We do this here since redirects and timeout will
	# lead to reinserting the identifier of the connection into
	# the queue, and we want to reinsert at the end.
	set cidx [lsearch $HostConn(queue) $cid]
	if { $cidx >= 0 } {
	    set HostConn(queue) [lreplace $HostConn(queue) $cidx $cidx]
	}

	# Decrease connection counts (outwards and for that host.
	incr HostConn(conns) -1
	incr MGU(conns) -1
	if { $Fetch(fdes) != "" } {
	    catch {close $Fetch(fdes)}
	}

	# Cancel possible inactivity check
	if { $Fetch(-maxinactivity) > 0 } {
	    after cancel Fetch(activitycheck)
	    set Fetch(activitycheck) ""
	}

	# Now analyse the results
	set status [::http::status $token]
	switch $status {
	    "ok" {
		set code [::http::ncode $token]
		switch -glob -- $code {
		    200 {
			__tellprogress $cid $urlstate(url) \
			    $urlstate(currentsize) $urlstate(totalsize)
			set keeptoken $Fetch(keeptoken)
			__done_finalize $cid "OK" $token
		    }
		    30[1237] {
			# Take care or redirects.  Change host queue
			# if we have arrived at another host.
			__tellprogress $cid $urlstate(url) \
			    $urlstate(currentsize) $urlstate(totalsize)
			if { $Fetch(-redirects) < 0 \
				 || [llength $Fetch(redirects)] \
				 < $Fetch(-redirects) } {
			    array set meta $urlstate(meta)
			    if { ![::info exists meta(Location)] } {
				${log}::warn \
				    "Error fetching $urlstate(url): missing\
                                     redirect information"
				set keeptoken $Fetch(keeptoken)
				__done_finalize $cid "ERROR" \
				    "No redirection for $urlstate(url)"
			    } else {
				set nexturl \
				    [::uri::resolve $urlstate(url) \
					 $meta(Location)]
				${log}::notice \
				    "Following redirect $urlstate(url)\
                                     -> $nexturl"
				lappend Fetch(redirects) $nexturl
				upvar \#0 [__new_host $nexturl] HostConn
				__enqueue HostConn(queue) $cid
				set Fetch(state) WAITING
				set Fetch(host) $HostConn(host)
			    }
			}
		    }
		    default {
			# Anything else is so far an error.
			${log}::warn \
			    "Error fetching $urlstate(url): $urlstate(http)"
			__tellprogress $cid $urlstate(url) \
			    $urlstate(currentsize) $urlstate(totalsize)
			set keeptoken $Fetch(keeptoken)
			__done_finalize $cid "ERROR" \
			    "HTTP Error for $urlstate(url): $urlstate(http)"
		    }
		}
	    }
	    "max inactivity" -
	    "error" -
	    "eof" -
	    "timeout" {
		# Errors or unexpected end of files will lead to retries.
		incr Fetch(retries)
		if { $Fetch(-retries) < 0 \
			 || $Fetch(retries) < $Fetch(-retries) } {
		    ${log}::notice "$status reached when fetching\
                                    $urlstate(url), retrying\
                                    $Fetch(retries) / $Fetch(-retries)"
		    set Fetch(state) "WAITING"
		    # Replace the fetch at the end of the queue
		    __enqueue HostConn(queue) $cid
		} else {
		    ${log}::notice "$status reached when fetching\
                                    $urlstate(url), no more retries"
		    set keeptoken $Fetch(keeptoken)
		    __done_finalize $cid "ERROR" \
			"Max retries $Fetch(-retries) reached"
		}
	    }
	    "cancel" {
		${log}::notice "$urlstate(url) fetching cancelled"
		set keeptoken $Fetch(keeptoken)
		__done_finalize $cid "CANCEL" "Cancelled"
	    }
	}
    } else {
	${log}::warn "$cid is not a recognised connection anymore!"
    }

    if { ! $keeptoken } {
	::http::cleanup $token
    }
}


# ::massgeturl::queuesize -- Get current queue size
#
#	Get the current size of the queue for one or several hosts
#	(matching a pattern).
#
# Arguments:
#	loglvl	New loglevel
#
# Results:
#	Return the current log level
#
# Side Effects:
#	None.
proc ::massgeturl::queuesize { { stateptn * } { hostptn * } } {
    variable MGU
    variable log

    set qsize 0
    foreach host $MGU(hosts) {
	if { [string match $hostptn $host] } {
	    set connid [::uid::id $MGU(uids) $host]
	    set varname "::massgeturl::HostConn_${connid}"
	    upvar \#0 $varname HostConn

	    foreach cid $HostConn(queue) {
		set varname "::massgeturl::Fetch_${cid}"
		upvar \#0 $varname Fetch

		if { [string match $stateptn $Fetch(state)] } {
		    incr qsize
		}
	    }
	}
    }

    return $qsize
}


# ::massgeturl::__queue_check -- Check pending queue
#
#	This command checks the content of the pending queue for a
#	given host or all known hosts.  Whenever there is space for
#	another get operation, the command will perform it in an
#	asynchronous manner.
#
# Arguments:
#	host	Name of host to examine queue of
#
# Results:
#	Return the number of geturl operations that were scheduled in
#	total.
#
# Side Effects:
#	Will call the http package to get the URL content, perhaps
#	several times.
proc ::massgeturl::__queue_check { { host "" } } {
    variable MGU
    variable log

    set scheduled 0

    # First build a list with all the name of the hosts for which we
    # want to examine the queue.
    if { $host == "" } {
	if { ! [__checkqueues] && 0 } {
	    ${log}::debug "Hinted not to check queues for now"
	    return
	}
	set hosts $MGU(hosts)
    } else {
	set host [string tolower $host]
	set idx [lsearch $MGU(hosts) $host]
	if { $idx >= 0 } {
	    set hosts $host
	} else {
	    ${log}::warn "Host $host unknown"
	}
    }
    
    # Now gather all connections that are associated to these hosts
    # into "candidates"
    set candidates ""
    foreach host $hosts {
	set host [string tolower $host]
	set connid [::uid::id $MGU(uids) $host]
	set varname "::massgeturl::HostConn_${connid}"
	upvar \#0 $varname HostConn
	set candidates [concat $candidates $HostConn(queue)]
    }
    if { 0 } {
	${log}::debug \
	    "Checking queues for $hosts: [llength $candidates] candidates"
    }
    
    # Now sort the list of candidates with respect to priorities.
    set sorted_candidates ""
    foreach id $candidates {
	__enqueue sorted_candidates $id
    }

    # For all possible connections that we should look into, look for
    # those at the highest priorities that belong to a host that has
    # space for more outbound connections.  If we find such, start
    # getting the URL content.
    foreach cid $sorted_candidates {
	set varname "::massgeturl::Fetch_${cid}"
	if { [::info exists $varname] } {
	    upvar \#0 $varname Fetch

	    set url $Fetch(url)
	    if { [llength $Fetch(redirects)] > 0 } {
		set url [lindex $Fetch(redirects) end]
	    }
	    
	    upvar \#0 [__new_host $url] HostConn
	    if { $Fetch(state) == "WAITING" } {
		if { ( $MGU(-maxhostconns) < 0 \
			   || $HostConn(conns) < $MGU(-maxhostconns)) \
			 && ( $MGU(-maxconns) < 0 \
				  || $MGU(conns) < $MGU(-maxconns) ) } {
		    set doit 1
		    set Fetch(fdes) ""
		    
		    # Extract authentication URL information from string
		    set exp {^(([^:]*)://)?([^@]+@)?([^/:]+)(:([0-9]+))?(/.*)?$}
		    if {![regexp -nocase $exp $url x prefix proto \
			      user host y port srvurl]} {
			${log}::warn "Could not extract split information\
                                      from url: $url"
			__done_finalize $cid "ERROR" \
			    "Could not analyse URL $url"
			set doit 0
		    }

		    if { $Fetch(-preparecmd) ne "" } {
			${log}::debug "Preparing to fetch\
                                       [::htauth::obfuscate $url]"
			if { [catch {eval [linsert $Fetch(-preparecmd) end \
					       $cid $url]} err] } {
			    ${log}::warn "Failed when calling preparing cmd:$err"
			    set doit 0
			}
		    }

		    if { $Fetch(type) == "CHANNEL" && $Fetch(fname) != "" \
			 && $doit } {
			if { [catch {::open $Fetch(fname) w} \
				  Fetch(fdes)] } {
			    ${log}::warn "Could not open $Fetch(fname)\
                                                  for writing: $Fetch(fdes)"
			    __done_finalize $cid "ERROR" \
				"Could not open output file\
                                         $Fetch(fname): $Fetch(fdes)"
			    set doit 0
			}
		    }
		    
		    if { $doit } {
			set cmd [list ::http::geturl $url \
				     -progress \
				     [list ::massgeturl::__progress $cid] \
				     -command \
				     [list ::massgeturl::__done $cid]]
			set hdrs [::htauth::headers $url]
			lappend cmd -headers $hdrs
			if { $Fetch(-timeout) >= 0 } {
			    lappend cmd -timeout $Fetch(-timeout)
			}
			if { $Fetch(type) == "HEAD" } {
			    lappend cmd -validate 1
			}
			if { $Fetch(type) == "CHANNEL" } {
			    lappend cmd -channel $Fetch(fdes)
			}
			set Fetch(state) "FETCHING"
			if { [catch {eval $cmd} token] } {
			    ${log}::warn \
				"Could not get URL $url: $token"
			    __done_finalize $cid "ERROR" \
				"Could not start fetch of $url: $token"
			} else {
			    __tellprogress $cid $url 0 -1
			    if { $Fetch(-maxinactivity) > 0 } {
				set Fetch(lastactive) \
				    [clock clicks -milliseconds]
				set Fetch(activitycheck) \
				    [after $Fetch(-maxinactivity) \
					 ::massgeturl::__checkinactivity $cid]
			    }
			    set Fetch(httoken) $token
			    incr scheduled
			    incr HostConn(conns)
			    incr MGU(conns)
			}
		    }
		}
	    }
	}
    }

    if { $scheduled > 0 } {
	${log}::debug \
	    "Queues checked ($hosts): $scheduled operation(s) scheduled"
	set MGU(qcheck_id) [after idle ::massgeturl::__queue_check]
    } else {
	#__checkqueues off
	if { [llength $sorted_candidates] > 0 } {
	    if { 0 } {
		${log}::debug \
		    "Queues checked ($hosts): no operations scheduled"
	    }
	    set MGU(qcheck_id) \
		[after $MGU(qcheck_pulse) ::massgeturl::__queue_check]
	}
    }

    return $scheduled
}



# ::massgeturl::__new_host -- Create/Reuse a host connection
#
#	This command creates and initalises a host connection.  If a
#	connection already exists for that URL, its variable is
#	returned.
#
# Arguments:
#	url	URL to get
#
# Results:
#	The name of the variable for the host connection
#
# Side Effects:
#	None.
proc ::massgeturl::__new_host { url } {
    variable MGU
    variable log

    # Extract host information from URL. We used previously uri::split
    # for that purpose, but it performs a complete analysis of the URL
    # that we do not need and is CPU intensive.  The following regular
    # expression is shamelessly borrowed from the HTTP library.
    set exp {^(([^:]*)://)?([^@]+@)?([^/:]+)(:([0-9]+))?(/.*)?$}
    if {![regexp -nocase $exp $url x prefix proto user host y port srvurl]} {
	${log}::warn "Could not extract host information from url: $url"
	return -code error "Could not extract host information from url: $url"
    }
    set host [string tolower $host]

    set connid [::uid::id $MGU(uids) $host]
    set varname "::massgeturl::HostConn_${connid}"

    # If this is a new host, initiate.  We test through the existence
    # of the variable (vs. searching MGU(hosts) for the host) for
    # speed reasons.
    if { ! [::info exists $varname] } {
	upvar \#0 $varname HostConn

	set HostConn(queue) ""
	set HostConn(conns) 0
	set HostConn(host) $host

	lappend MGU(hosts) $HostConn(host)
	${log}::info "Created new host context for $host\
                      ([llength $MGU(hosts)] known hosts)"
    }

    return $varname
}


# ::massgeturl::__enqueue -- Priority-dependent enqueuing
#
#	This command adds a connection to a list of connections with
#	regard to its priority and the priorities of the remaining
#	connections in the queue.  Connections with higher priorities
#	will be inserted at the head of the queue.
#
# Arguments:
#	queue_p	Pointer to queue into which to add connection
#	cid	Identifier of connection to add.
#
# Results:
#	The index of the connection in the queue
#
# Side Effects:
#	None.
proc ::massgeturl::__enqueue { queue_p cid } {
    upvar $queue_p queue

    set varname "::massgeturl::Fetch_${cid}"
    upvar \#0 $varname Fetch

    set i 0
    foreach id $queue {
	set varname "::massgeturl::Fetch_${id}"
	upvar \#0 $varname OtherFetch
	if { $OtherFetch(-priority) < $Fetch(-priority) } {
	    break
	}
	incr i
    }

    set queue [linsert $queue $i $cid]

    return $i
}


# ::massgeturl::__new_fetch -- Create/Reuse a fetch operation
#
#	This command creates and initalises a fetch operation.  If an
#	identical operation already exists for that URL, the callback
#	command is simply added to the list of callback for the
#	operation corresponding to that URL.
#
# Arguments:
#	url	URL to get
#	cmd	Command to call on completion (see parameters above)
#	type	Type of operation (GET, HEAD or CHANNEL)
#	fname	Name of file for URL content storage
#
# Results:
#	The identifier of the new/old connection/operation.
#
# Side Effects:
#	None.
proc ::massgeturl::__new_fetch { url cmd type { fname "" } } {
    variable MGU
    variable log

    # Convert URL to translate characters in official characters.
    set url [urlencode $url]

    # Look if we don't already have a fetch of the same type for that URL
    set already 0
    foreach cid $MGU(connections) {
	set varname "::massgeturl::Fetch_${cid}"
	upvar \#0 $varname Fetch

	if { $Fetch(url) == $url && $Fetch(type) == $type } {
	    set already 1
	    break
	}
    }
    
    if { ! $already } {
	# Now create a connection identifier and push a request in the queue.
	set cid [incr MGU(idgene)]
	set varname "::massgeturl::Fetch_${cid}"
	upvar \#0 $varname Fetch

	upvar \#0 [__new_host $url] HostConn
	
	set Fetch(url) $url
	set Fetch(host) $HostConn(host)
	set Fetch(id) $cid
	set Fetch(retries) 0
	set Fetch(redirects) ""
	set Fetch(state) "WAITING"
	set Fetch(type) $type
	set Fetch(fname) $fname
	set Fetch(fdes) ""
	set Fetch(callbacks) [list $cmd]
	set Fetch(httoken) ""
	set Fetch(keeptoken) 0
	set Fetch(dn_lastout) [clock clicks -milliseconds]
	foreach opt $MGU(inheritance) {
	    set Fetch($opt) $MGU($opt)
	}

	__enqueue MGU(connections) $cid
	__enqueue HostConn(queue) $cid
	#__checkqueues on
    } else {
	set varname "::massgeturl::Fetch_${cid}"
	upvar \#0 $varname Fetch
	
	lappend Fetch(callbacks) $cmd
    }

    return $cid
}


# ::massgeturl::config -- Configure a get operation
#
#	This command set or get the options of a get operation.
#
# Arguments:
#	cid	Connection identifier
#	args	list of options (-timeout -retries -redirects)
#
# Results:
#	Return all options, the option requested or set the options
#
# Side Effects:
#	None.
proc ::massgeturl::config { cid args } {
    variable MGU
    variable log

    # Check that this is one of our connections
    set idx [lsearch $MGU(connections) $cid]
    if { $idx < 0 } {
	${log}::warn "Mass get URL identifier $cid is not valid"
	return -code error "Identifier invalid"
    }

    set varname "::massgeturl::Fetch_${cid}"
    upvar \#0 $varname Fetch

    set o [lsort [array names Fetch "-*"]]

    if { [llength $args] == 0 } {      ;# Return all results
	set result ""
	foreach name $o {
	    lappend result $name $Fetch($name)
	}
	return $result
    }

    set sort_again 0
    foreach {opt value} $args {        ;# Get onr or set some
	if { [lsearch $o $opt] == -1 } {
	    return -code error "Unknown option $opt, must be: [join $o ", " ]"
	}
	if { [llength $args] == 1 } {  ;# Get one config value
	    return $Fetch($opt)
	}
	set Fetch($opt) $value         ;# Set the config value
	if { $opt == "-priority" } {
	    set sort_again 1
	}
    }

    if { $sort_again } {
	set newconnections ""
	foreach id $MGU(connections) {
	    __enqueue newconnections $id
	}
	set MGU(connections) $newconnections

	foreach host $MGU(hosts) {
	    set host [string tolower $host]
	    set connid [::uid::id $MGU(uids) $host]
	    set varname "::massgeturl::HostConn_${connid}"
	    upvar \#0 $varname HostConn
	    set newqueue ""
	    foreach id $HostConn(queue) {
		__enqueue newqueue $id
	    }
	    set HostConn(queue) $newqueue
	}
    }
}


# ::massgeturl::get -- Asynchronously get a URL
#
#	This command performs a URL get request asynchronously and
#	arranges for a command to be called when the content has been
#	fetched or when an error has occured.  Redirections will be
#	followed, timeouts will lead to retries. This command can be
#	called several time for the same URL, in which case all the
#	commands specified will be called in order upon completion.
#	The command called back will take the following arguments:
#	identifier of the connection (get operation), (original) URL
#	being fetched, a status (OK or ERROR), the URL fetching token,
#	as returned by the last ::http::geturl or an explaining text
#	in case of errors.
#
# Arguments:
#	url	URL to get
#	cmd	Command to call on completion (see parameters above)
#	args	list of options (-timeout -retries -redirects)
#
# Results:
#	Return an identifier for that connection.  This parameter can
#	be used to modify the get operation straight afterwards (since
#	the get operation is only scheduled).
#
# Side Effects:
#	Will call the http package to get the URL content, perhaps
#	several times.
proc ::massgeturl::get { url cmd args } {
    variable MGU
    variable log

    set cid [__new_fetch $url $cmd "GET"]
    eval config $cid $args
    if { $MGU(qcheck_id) ne "" } {
	after cancel $MGU(qcheck_id)
    }
    set MGU(qcheck_id) [after idle ::massgeturl::__queue_check]
    
    return $cid
}


# ::massgeturl::infile -- Asynchronously get a URL into a local file
#
#	This command performs a URL get request asynchronously and
#	arranges for a command to be called when the content has been
#	fetched into a given channel or when an error has occured.
#	Redirections will be followed, timeouts will lead to
#	retries. This command can be called several time for the same
#	URL, in which case all the commands specified will be called
#	in order upon completion.  The command called back will take
#	the following arguments: identifier of the connection (get
#	operation), (original) URL being fetched, a status (OK or
#	ERROR), the URL fetching token, as returned by the last
#	::http::geturl or an explaining text in case of errors.
#
# Arguments:
#	url	URL to get
#	fname	Name of file into which to store the content
#	cmd	Command to call on completion (see parameters above)
#	args	list of options (-timeout -retries -redirects)
#
# Results:
#	Return an identifier for that connection.  This parameter can
#	be used to modify the get operation straight afterwards (since
#	the get operation is only scheduled).
#
# Side Effects:
#	Will call the http package to get the URL content, perhaps
#	several times.
proc ::massgeturl::infile { url fname cmd args } {
    variable MGU
    variable log

    set cid [__new_fetch $url $cmd "CHANNEL" $fname]
    eval config $cid $args
    if { $MGU(qcheck_id) ne "" } {
	after cancel $MGU(qcheck_id)
    }
    set MGU(qcheck_id) [after idle ::massgeturl::__queue_check]
    
    return $cid
}


# ::massgeturl::head -- Asynchronously poll for a URL
#
#	This command performs a URL valisation request asynchronously
#	and arranges for a command to be called when the URL has been
#	validated or when an error has occured.  Redirections will be
#	followed, timeouts will lead to retries. This command can be
#	called several time for the same URL, in which case all the
#	commands specified will be called in order upon completion.
#	The command called back will take the following arguments:
#	identifier of the context, identifier of the connections (get
#	operation), (original) URL being fetched, a status (OK or
#	ERROR), the URL fetching token, as returned by the last
#	::http::geturl or an explaining text in case of errors
#
# Arguments:
#	url	URL to poll for
#	cmd	Command to call on completion (see parameters above)
#	args	list of options (-timeout -retries -redirects)
#
# Results:
#	Return an identifier for that connection.  This parameter can
#	be used to modify the get operation straight afterwards (since
#	the get operation is only scheduled).
#
# Side Effects:
#	Will call the http package to get the URL content, perhaps
#	several times.
proc ::massgeturl::head { url cmd args } {
    variable MGU
    variable log

    set cid [__new_fetch $url $cmd "HEAD"]
    eval config $cid $args
    if { $MGU(qcheck_id) ne "" } {
	after cancel $MGU(qcheck_id)
    }
    set MGU(qcheck_id) [after idle ::massgeturl::__queue_check]
    
    return $cid
}


# ::massgeturl::__sync_done -- Finalise synchronised URL fetch
#
#	This command finalises a synchronised URL fetch through
#	mediating the result of the fetch to the caller via the state
#	array for the synchronisation.
#
# Arguments:
#	sid	Identifier of the synchronisation
#	cid	Identifier of the URL fetch connection
#	url	URL to get/poll for
#	status	Result of the get/head
#	token	Content of the URL or error
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::massgeturl::__sync_done { sid cid url status token } {
    set varname "::massgeturl::Sync_${sid}"
    upvar \#0 $varname Sync

    if { $status == "OK" } {
	set Sync(httoken) $token
    }

    set Sync(finished) 1
}


# ::massgeturl::__sync_get -- Synchronised URL fetch
#
#	This command sets up a synchronised URL fetch (get or poll)
#	through the creation of a synchronisation array that will be
#	used both to convey the result of the get and the state of the
#	synchronisation.
#
# Arguments:
#	cmd	Command to perform (get or head)
#	url	URL to get/poll for
#	args	Additional arguments to the command
#
# Results:
#	Identifier of the synchronisation array.
#
# Side Effects:
#	Will block the process until all redirection/timeouts followed
proc ::massgeturl::__sync_get { cmd url args } {
    variable MGU
    variable log

    # Create a new synchronisation array
    set sid [incr MGU(idgene)]
    set varname "::massgeturl::Sync_${sid}"
    upvar \#0 $varname Sync

    # Fill in the array with all necessary arguments.  See to give to
    # get or head the __sync_done command which will communicate back
    # to us through this synchronisation array ("finished" key).
    set sync_cmd [list ::massgeturl::__sync_done $sid]
    set Sync(id) $sid
    set Sync(cid) [eval $cmd {$url} {$sync_cmd} $args]
    set Sync(finished) 0
    set Sync(httoken) ""

    # Now force the get/head command not to remove the http token on
    # success, this will be up to the caller.
    set varname "::massgeturl::Fetch_$Sync(cid)"
    upvar \#0 $varname Fetch
    set Fetch(keeptoken) 1

    # And wait for completion...
    vwait ::massgeturl::Sync_${sid}(finished)
    
    return $sid
}


# ::massgeturl::syncget -- Synchronously get a URL
#
#	This command performs a URL get request synchronously and will
#	return the http token upon success. Redirections will be
#	followed, timeouts will lead to retries.
#
# Arguments:
#	url	URL to get
#	args	list of options (-timeout -retries -redirects)
#
# Results:
#	Return the http token for the (followed) URL or an empty string
#
# Side Effects:
#	Will call the http package to get the URL content, perhaps
#	several times. Will block the process until all web operations
#	for that URL have been completed.
proc ::massgeturl::syncget { url args } {
    set sid [eval __sync_get ::massgeturl::get {$url} $args]
    set varname "::massgeturl::Sync_${sid}"
    upvar \#0 $varname Sync
    set token $Sync(httoken)

    unset Sync
    return $token
}


# ::massgeturl::synchead -- Synchronously poll for a URL
#
#	This command performs a URL valisation request synchronously
#	and will return the http token upon success. Redirections will
#	be followed, timeouts will lead to retries.
#
# Arguments:
#	url	URL to poll for
#	args	list of options (-timeout -retries -redirects)
#
# Results:
#	Return the http token for the (followed) URL or an empty string
#
# Side Effects:
#	Will call the http package to get the URL content, perhaps
#	several times. Will block the process until all web operations
#	for that URL have been completed.
proc ::massgeturl::synchead { url args } {
    set sid [eval __sync_get ::massgeturl::head {$url} $args]
    set varname "::massgeturl::Sync_${sid}"
    upvar \#0 $varname Sync
    set token $Sync(httoken)

    unset Sync
    return $token
}


# ::massgeturl::urlencode -- Encode a URL
#
#	This command encodes a string so that it complies to RFC 2396,
#	i.e. removes all forbidden characters.
#
# Arguments:
#	url	URL to encode
#
# Results:
#	Encoded URL.
#
# Side Effects:
#	None.
proc ::massgeturl::urlencode { url } {
    variable MGU

    if { [llength $MGU(enc_charmap)] == 0 } {
	for {set i 0} {$i <= 256} {incr i} {
	    set c [format %c $i]
	    if {![string is alnum $c] \
		    && ![string match {[;/?:@&=+$,-_.!~*'()]} $c]} {
		set encoded %[format %.2x $i]
		set map($c) $encoded
		set revmap($encoded) $c
	    }
	    set MGU(enc_charmap) [array get map]
	    set MGU(dec_charmap) [array get revmap]
	}
    }

    # The algorithm consists in first decoding back the URL and
    # reencoding it.  By doing this we ensure that the special
    # characters created by the encoding will not be encoded
    # themselves (otherwise ' ' would become %20 would become %25%20
    # and so on!!!!)
    return [string map $MGU(enc_charmap) [string map $MGU(dec_charmap) $url]]
}


# ::massgeturl::cancel -- Cancel a download
#
#	This command cancel one or several currently scheduled URL
#	fetch, it will call the associated callbacks with the CANCEL
#	argument.
#
# Arguments:
#	args	Connection identifiers, returned by ::get or ::head
#
# Results:
#	None.
#
# Side Effects:
#	Will cancel possible on-going downloads.
proc ::massgeturl::cancel { args } {
    variable MGU
    variable log

    foreach cid $args {
	set idx [lsearch $MGU(connections) $cid]
	if { $idx >= 0 } {
	    # Get to information about the fetch
	    set varname "::massgeturl::Fetch_${cid}"
	    upvar \#0 $varname Fetch
	    
	    if { $Fetch(state) == "WAITING" } {
		__done_finalize $cid "CANCEL" "Cancelled"
	    } else {
		::http::reset $Fetch(httoken) "cancel"
	    }
	}
    }
}
