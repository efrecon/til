# rsswatch.tcl --
#
#	This modules provides a set of routines to provide a cron-like
#	facility.
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

package require Tcl 8.2
package require logger
package require http
package require rssparser

package provide rsswatch 1.0

namespace eval ::rsswatch {
    # Variables of name ::rsswatch::Feed_<id> are created as arrays to
    # support each feed monitoring.

    # Initialise the global state
    variable RSSW
    if {![::info exists RSSW]} {
	array set RSSW {
	    feeds        ""
	    feed_id      0
	    loglevel     warn
	    -dftperiod   30
	    -geturl      ::rsswatch::__geturl
	    -async       on
	    -diff        "title link description enclosure"
	}
	variable log [::logger::init rsswatch]
	${log}::setlevel $RSSW(loglevel)
    }

    namespace export loglevel feedinfo new
}

# XXX: We should make a parameter with what we use for checking the
# equality of items.  title, description, link, enclosure, etc.  Easy:
# make it a list of things to test.

# ::rsswatch::loglevel -- Set/Get current log level.
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
proc ::rsswatch::loglevel { { loglvl "" } } {
    variable RSSW
    variable log

    if { $loglvl != "" } {
	if { [catch "${log}::setlevel $loglvl"] == 0 } {
	    set RSSW(loglevel) $loglvl
	}
    }

    return $RSSW(loglevel)
}


# ::rsswatch::__find_feed -- Find known feed
#
#	Find a feed by its URL
#
# Arguments:
#	url	URL of feed to find
#
# Results:
#	Return the current feed identifier or an empty string
#
# Side Effects:
#	None.
proc ::rsswatch::__find_feed { url } {
    variable RSSW

    foreach id $RSSW(feeds) {
	set varname ::rsswatch::Feed_$id
	upvar \#0 $varname Feed
	if { $Feed(url) == $url } {
	    return $id
	}
    }

    return ""
}


# ::rsswatch::__analyse_feed -- Start feed analysis
#
#	Command that satisfies the arguments imposed by
#	::http::geturl.  Check the content of the body and callback
#	the command that was given to us in the arguments.
#
# Arguments:
#	cmd	Command to callback with data once got.
#	httoken	HTTP token as sent back by ::http::geturl
#
# Results:
#	None
#
# Side Effects:
#	None
proc ::rsswatch::__analyse_feed { cmd httoken } {
    variable log
    variable RSSW

    # ::rsswatch::__analyse_feed is called as a callback from the
    # ::http::geturl command, retrieve information from the body only
    # if we have something to get.
    set rss ""
    upvar \#0 $httoken ht_state
    if { $ht_state(status) == "ok" && [::http::ncode $httoken] == 200 } {
	set rss $ht_state(body)
    } else {
	${log}::warn \
	    "Could not get content of $Feed(url): [::http::error $httoken]"
    }
    ::http::cleanup $httoken
    
    # Now callback command with the content, possibly empty.
    if { [catch {eval $cmd {$rss}} err] } {
	${log}::warn "Error when starting data analysis: $err"
    }
}


# ::rsswatch::__geturl -- Trigger URL fetching
#
#	Wrapper command around ::http::geturl to fit in the callback
#	mechanism.  Handle everything which does not start with http
#	as a local file, which is completely wrong but can do as a
#	start.  Maybe we should look into using uri::geturl instead,
#	but the implementation is pretty inexistent at this time.
#
# Arguments:
#	id	Identifier of feed to check
#	url	URL to fetch
#	cmd	Command to callback with data once got.
#
# Results:
#	None
#
# Side Effects:
#	None
proc ::rsswatch::__geturl { id url cmd } {
    variable log

    if { [string match -nocase "http:/*" $url] } {
	if { [catch {::http::geturl $url \
			 -binary on \
			 -command [list ::rsswatch::__analyse_feed \
				       $cmd]} err] } {
	    ${log}::warn "Error when inialising URL fetch: $err"
	}
    } else {
	if { [catch {open $url} fdes] } {
	    set rss ""
	    ${log}::warn "Could not open local file $url: $fdes"
	} else {
	    set rss [read $fdes]
	    close $fdes
	}
	
	# See to always call the command cmd, otherwise the
	# periodicity will be lost since __check_feed reposts itself
	# with a timer.
	if { [catch {eval $cmd {$rss}} err] } {
	    ${log}::warn "Error when starting data analysis: $err"
	}
    }
}


# ::rsswatch::__feeds_diff -- Compute feed differences
#
#	Discover the new items in a feed, as compared to an old one,
#	and deliver the callback as appropriate.  Feed items are
#	compared using the title, link description and enclosure.
#
# Arguments:
#	parser	RSS parser
#	rssnew	Newly income RSS feed description (parsed)
#	rssold	Old income RSS feed description (parsed)
#	cmd	Command to callback
#	id	Identifier of RSS feed monitor
#	op	Name of operation to deliver to command
#
# Results:
#	Number of differences
#
# Side Effects:
#	Will do the appropriate callbacks
proc ::rsswatch::__feeds_diff { parser rssnew rssold cmd id op } {
    variable log

    # Get to the information about the feed from its identifier.
    set varname ::rsswatch::Feed_$id
    upvar \#0 $varname Feed

    set varname [::rssparser::get_element $parser $rssnew]
    upvar \#0 $varname Channel

    set diffs 0
    set items $Channel(items)
    set l [llength $items]
    while { $l } {
	set item [lindex $items [incr l -1]]
	set varname [::rssparser::get_element $parser $item]
	upvar \#0 $varname Item
	set existed 0
	set oc_varname \
	    [::rssparser::get_element $parser $rssold]
	upvar \#0 $oc_varname OldChannel
	foreach oldid $OldChannel(items) {
	    set o_varname \
		[::rssparser::get_element $parser $oldid]
	    upvar \#0 $o_varname OldItem
	    
	    set equal 1
	    foreach key $Feed(-diff) {
		if { [array names Item $key] == "" \
			 && [array names OldItem $key] != "" } {
		    set equal 0
		    break
		} elseif { [array names Item $key] != "" \
			       && [array names OldItem $key] == "" } {
		    set equal 0
		    break
		} elseif { [array names Item $key] != "" \
			       && [array names OldItem $key] != "" \
			       && $OldItem($key) != $Item($key) } {
		    set equal 0
		    break
		}
	    }
	    if { $equal } {
		set existed 1
		break
	    }
	}
	if { ! $existed } {
	    incr diffs
	    ${log}::debug "Item $Item(link)/$Item(title): $op"
	    if { [catch {eval $cmd $id $op $varname} err] } {
		${log}::warn \
		    "Error when invoking RSS callback: $err"
	    }
	} else {
	    ${log}::debug "Item $Item(link) already present"
	}
    }

    return $diffs
}


# ::rsswatch::__check_feed -- Check feed for new items.
#
#	Check feed XML data for new items and deliver these as callbacks
#
# Arguments:
#	id	Identifier of feed to check
#	data	RSS data to analyse
#
# Results:
#	None
#
# Side Effects:
#	Will do the appropriate callbacks
proc ::rsswatch::__check_feed { id vnum data } {
    variable log
    variable RSSW

    # Get to the information about the feed from its identifier.
    set varname ::rsswatch::Feed_$id
    upvar \#0 $varname Feed

    if { $Feed(nbchecks) != $vnum } {
	${log}::warn "Wrong version number for feed.  Is $Feed(nbchecks),\
                      should be $vnum.  Discarding!"
	set rss ""
    } else {
	${log}::notice "Analysing feed info at $Feed(url)"
	#XXX: Enable the following line to "see" the RSS content (big output!)
	#${log}::debug "Checking new RSS content:\n${data}"
	if { $data != "" } {
	    if { [catch {::rssparser::parse $Feed(parser) $data} rss] } {
		${log}::warn "Error when parsing feed at $Feed(url): $rss"
	    }
	} else {
	    set rss ""
	}
    }

    # We managed to get some date and to parse something from it.
    # Either we initialise the feed content if nothing has ever been
    # delivered, either we compute the difference between the previous
    # content and the new content and deliver callbacks accordingly.
    if { $rss != "" } {
	set varname [::rssparser::get_element $Feed(parser) $rss]
	upvar \#0 $varname Channel

	foreach key [list title description link ttl] {
	    set Feed($key) $Channel($key)
	}

	# Dynamically reflect period from RSS feed into our vision of
	# things.
	if { $Feed(ttl) != "" && [string is integer $Feed(ttl)] } {
	    set Feed(period) $Feed(ttl)
	    ${log}::info \
		"Dynamically changed check period for $Feed(title)\
                 to $Feed(period) min."
	} elseif { $Feed(period) != $Feed(-dftperiod) } {
	    # Use the default period, we might have changed it since
	    # we created the RSS monitor.
	    set Feed(period) $Feed(-dftperiod)
	    ${log}::info \
		"Using back default period for $Feed(title), i.e.\
                 $Feed(period) min."
	}

	# We have done feed fetching and parsing once, either this
	# was successfull and we will compute the difference or it
	# was not and we deliver new callbacks.
	if { $Feed(rss) == "" } {
	    # We had no information yet about the feed content,
	    # deliver initialisation callbacks for all current
	    # items.
	    ${log}::debug "Initialising feed content"
	    set items $Channel(items)
	    set l [llength $items]
	    while { $l } {
		set item [lindex $items [incr l -1]]
		set varname [::rssparser::get_element $Feed(parser) $item]
		if { [catch {eval $Feed(cmd) $id "NEW" $varname} err] } {
		    ${log}::warn \
			"Error when invoking RSS callback on $Feed(url): $err"
		}
	    }
	} else {
	    # We have already fetched information from the stream,
	    # this is the most usual case.  Compute the difference
	    # between what we knew last time and what we know now,
	    # deliver callbacks for items that have newly been added
	    # to the feed or that have been modified (this could be
	    # further checked through checking the time of their
	    # permalinks perhaps).  We reverse the new and old
	    # concepts to decide which have gone.
	    ${log}::debug "Computing difference for feed"
	    __feeds_diff $Feed(parser) $rss $Feed(rss) $Feed(cmd) $id "NEW"
	    __feeds_diff $Feed(parser) $Feed(rss) $rss $Feed(cmd) $id "REMOVE"
	}
    } else {
	${log}::debug "No feed data to analyse for $Feed(url)"
    }


    if { $vnum == $Feed(nbchecks) } {
	set when [expr int($Feed(period) * 60000)]
	${log}::debug "Scheduling next check for $Feed(url) in $when msecs"
	set vernum [expr $Feed(nbchecks) + 1]
	set checkcmd "::rsswatch::__check_feed $id $vernum"
	set Feed(pollid) \
	    [after $when [list $Feed(-geturl) $id $Feed(url) $checkcmd]]
	
	if { [array names Feed rss] == "" } {
	    set Feed(rss) $rss
	} elseif { $rss != "" } {
	    if { $Feed(rss) != "" } {
		::rssparser::destroy $Feed(parser) $Feed(rss)
	    }
	    set Feed(rss) $rss
	}

	${log}::debug "We have checked the feed $vernum time(s)"
	incr Feed(nbchecks)
    } else {
	${log}::warn "Version number mismatch, no scheduling this time!"
    }
}


# ::rsswatch::feedinfo -- Return feed info
#
#	Get specific feed information and return it.  The valid keys
#	for the time being are title link and description.
#
# Arguments:
#	id	Identifier of feed to check
#	info	Key to get value for (title, link or description)
#
# Results:
#	The value of the information or an empty string.
#
# Side Effects:
#	None
proc ::rsswatch::feedinfo { id info } {
    variable RSSW

    set idx [lsearch $RSSW(feeds) $id]
    if { $idx < 0 } {
	return ""
    }

    set varname ::rsswatch::Feed_$id
    upvar \#0 $varname Feed

    if { [array names Feed $info] != "" } {
	return [set Feed($info)]
    } else {
	return ""
    }
}


# ::rsswatch::delete -- Delete an RSS monitor
#
#	Delete an RSS monitor that had previously been created,
#	registered callbacks will cease to be called and the reference
#	to the watch will get invalid.
#
# Arguments:
#	id	Identifier of watch
#
# Results:
#	Returns 0 on failure, 1 on success
#
# Side Effects:
#	None.
proc ::rsswatch::delete { id } {
    variable RSSW

    set idx [lsearch $RSSW(feeds) $id]
    if { $idx < 0 } {
	return 0
    }

    set varname ::rsswatch::Feed_$id
    upvar \#0 $varname Feed

    if { $Feed(pollid) != "" } {
	after cancel $Feed(pollid)
    }
    ::rssparser::delete $Feed(parser)
    
    set RSSW(feeds) [lreplace $RSSW(feeds) $idx $idx]
    unset Feed
    
    return 1
}


# ::rsswatch::new -- Start monitoring a feed
#
#	Start monitoring a feed and arrange for a command to be called
#	back for every change that has beed discovered in the feed.
#	The command will be called with the following arguments:
#	identifier of the monitoring, operation (NEW or DELETE) and a
#	token for the item.  Do upvar #0 $token to access its content.
#
# Arguments:
#	url	URL of the feed to watch
#	cmd	Command to call back on each newly discovered item
#	args	List of key values for configuration (see config)
#
# Results:
#	Returns an identifier for this monitoring that will be used in
#	all further calls to the library.
#
# Side Effects:
#	None
proc ::rsswatch::new { url cmd args } {
    variable RSSW
    variable log
    
    set id [::rsswatch::__find_feed $url]
    if { $id == "" } {
	set id [incr RSSW(feed_id)]

	set varname ::rsswatch::Feed_$id
	upvar \#0 $varname Feed

	set Feed(url) $url
	set Feed(id) $id
	lappend RSSW(feeds) $Feed(id)
	
	set Feed(cmd) $cmd
	set Feed(parser) [::rssparser::new]
	set Feed(rss) ""
	set Feed(pollid) ""
	set Feed(nbchecks) 0
	foreach opt [array names RSSW "-*"] {
	    set Feed($opt) $RSSW($opt)
	}
	eval config $id $args
	set Feed(period) $Feed(-dftperiod)

	foreach key [list title description link] {
	    set Feed($key) ""
	}

	${log}::debug "Creating new RSS feed monitoring on $url"
	set checkcmd "::rsswatch::__check_feed $id $Feed(nbchecks)"
	# Schedule away so that we will be able to use the identifier
	# of the watch in the callbacks.
	set Feed(pollid) [after idle $Feed(-geturl) $id $url [list $checkcmd]]
	if { ! [string is true $Feed(-async)] } {
	    vwait ::rsswatch::Feed_${id}(nbchecks)
	}
    } else {
	set varname ::rsswatch::Feed_$id
	upvar \#0 $varname Feed

	set Feed(cmd) $cmd
	eval config $id $args
    }

    return $id
}


# ::rsswatch::config -- Configure a RSS watch
#
#	This command set or get the options of a RSS watch.  The
#	options that are currently recognised are -dftperiod (default
#	period for feed fetch, in minutes), -geturl (command to call
#	fetch the remote RSS URL), -async (wait for first fetch to be
#	performed before going on).
#
# Arguments:
#	wid	Identifier of feed to check
#	args	list of options (-dftperiod, -geturl, -async)
#
# Results:
#	Return all options, the option requested or set the options
#
# Side Effects:
#	None.
proc ::rsswatch::config { wid args } {
    variable RSSW
    variable log

    # Check that this is one of our connections
    set idx [lsearch $RSSW(feeds) $wid]
    if { $idx < 0 } {
	${log}::warn "Watch identifier $wid is not valid"
	return -code error "Identifier invalid"
    }

    set varname "::rsswatch::Feed_${wid}"
    upvar \#0 $varname Feed

    set o [lsort [array names Feed "-*"]]

    if { [llength $args] == 0 } {      ;# Return all results
	set result ""
	foreach name $o {
	    lappend result $name $Feed($name)
	}
	return $result
    }

    foreach {opt value} $args {        ;# Get one or set some
	if { [lsearch $o $opt] == -1 } {
	    return -code error "Unknown option $opt, must be: [join $o ", " ]"
	}
	if { [llength $args] == 1 } {  ;# Get one config value
	    return $Feed($opt)
	}
	set Feed($opt) $value          ;# Set the config value
    }
}


# ::rsswatch::defaults -- Set/Get defaults for all new connections
#
#	This command sets or gets the defaults options for all new
#	monitoring, it will not perpetrate on existing pending
#	connections, use ::rsswatch::config instead.
#
# Arguments:
#	args	List of -key value or just -key to get value
#
# Results:
#	Return all options, the option requested or set the options
#
# Side Effects:
#	None.
proc ::rsswatch::defaults { args } {
    variable RSSW
    variable log

    set o [lsort [array names RSSW "-*"]]

    if { [llength $args] == 0 } {      ;# Return all results
	set result ""
	foreach name $o {
	    lappend result $name $RSSW($name)
	}
	return $result
    }

    foreach {opt value} $args {        ;# Get one or set some
	if { [lsearch $o $opt] == -1 } {
	    return -code error "Unknown option $opt, must be: [join $o ,]"
	}
	if { [llength $args] == 1 } {  ;# Get one config value
	    return $RSSW($opt)
	}
	set RSSW($opt) $value           ;# Set the config value
    }
}
