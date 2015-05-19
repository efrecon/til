# head.tcl --
#
#	This modules provides a set of routines to get and cache
#	generic information about remote URLs and local files through
#	issuing HEAD request (on remote URLs).
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

package require Tcl 8.2
package require logger
package require diskutil
package require mimetype
package require massgeturl
package require rescacher

package provide urlhead 1.0

namespace eval ::urlhead {
    variable UH
    if {![::info exists UH]} {
	array set UH {
	    loglevel      warn
	    idgene        0
	    -timeout      30000
	    -retries      3
	    -redirects    10
	    -maxcache     10
	    cache         ""
	    keys          "url mtime mimetype size endurl"
	}
	variable log [::logger::init [string trimleft [namespace current] ::]]
	${log}::setlevel $UH(loglevel)
    }

    namespace export loglevel config get find urlinfo hint
}


# ::urlhead::loglevel -- Set/Get current log level.
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
proc ::urlhead::loglevel { { loglvl "" } } {
    variable UH
    variable log

    if { $loglvl != "" } {
	if { [catch "${log}::setlevel $loglvl"] == 0 } {
	    set UH(loglevel) $loglvl
	}
    }

    return $UH(loglevel)
}


# ::urlhead::remove -- Remove known cache information
#
#	Remove known URL information identifier from the cache. 
#
# Arguments:
#	id	The identifier of the cache info, as returned by ::urlhead::get
#
# Results:
#	Return 1 if removed, 0 if not (not in cache, probably)
#
# Side Effects:
#	None.
proc ::urlhead::remove { id } {
    variable UH
    variable log

    set idx [lsearch $UH(cache) $id]
    if { $idx >= 0 } {
	set varname "::urlhead::URLInfo_${id}"
	upvar \#0 $varname URLInfo
	${log}::debug "Removed information for $URLInfo(url)"
	unset URLInfo
	set UH(cache) [lreplace $UH(cache) $idx $idx]
	
	return 1
    }

    return 0
}


# ::urlhead::find -- Find if a URL is in the cache.
#
#	Find if the cache knows specific information about a URL.
#
# Arguments:
#	url	URL to query
#
# Results:
#	Return the identifier of the information in the cache or an
#	empty string.
#
# Side Effects:
#	None.
proc ::urlhead::find { url } {
    variable UH
    variable log

    if { [string match -nocase "http:/*" $url] } {
	set url [::massgeturl::urlencode $url]
    }

    foreach id $UH(cache) {
	set varname "::urlhead::URLInfo_${id}"
	upvar \#0 $varname URLInfo
	if { $url eq $URLInfo(url) } {
	    return $id
	}
    }

    return ""
}


# ::urlhead::hint -- Store external http request information
#
#	Store (external) HTTP request information (deemed successful)
#	into the head information cache.
#
# Arguments:
#	url	URL that was queried
#	token	Token as returned by http::geturl
#	keys	Which information should we extract and analyse (mimetype,
#               mtime, size or endurl
#
# Results:
#	Return the list of keys that were successfully analysed and stored.
#
# Side Effects:
#	None.
proc ::urlhead::hint { url token { keys "" } } {
    variable UH
    variable log

    if { [string match -nocase "http:/*" $url] } {
	set url [::massgeturl::urlencode $url]
    }
    set id [find $url]
    if { $id == "" } {
	set id [incr UH(idgene)]
	set varname "::urlhead::URLInfo_${id}"
	upvar \#0 $varname URLInfo
	set URLInfo(url) $url
	set URLInfo(id) $id
	set URLInfo(ctime) [clock seconds]
	set URLInfo(state) "HINTED"
	# Get rid of oldest information from the cache if we have
	# reached the maximum size (and have such a maximum)
	if { $UH(-maxcache) >= 0 && [llength $UH(cache)] > $UH(-maxcache) } {
	    ${log}::debug "Making space for $url in cache"
	    remove [lindex $UH(cache) 0]
	}
	lappend UH(cache) $id
    }

    if { $keys == "" } {
	set keys $UH(keys)
    }
    upvar \#0 $token state
    set varname "::urlhead::URLInfo_${id}"
    upvar \#0 $varname URLInfo
    set analysed ""

    # MIME type
    if { [lsearch $keys "mimetype"] >= 0 } {
	set semicolon [string first ";" $state(type)]
	if { $semicolon >= 0 } {
	    set URLInfo(mimetype) \
		[string range $state(type) 0 [expr {$semicolon - 1}]]
	} else {
	    set URLInfo(mimetype) $state(type)
	}
	${log}::debug "Stored for $url: mimetype= $URLInfo(mimetype)"
	lappend analysed "mimetype"
    }

    # Modification date
    if { [lsearch $keys "mtime"] >= 0 } {
	array set meta $state(meta)
	set mtime 0
	if { [array names meta "Last-Modified"] == "Last-Modified" } {
	    if { [catch "clock scan \"$meta(Last-Modified)\"" \
		      mtime] == 0 } {
		lappend analysed "mtime"
	    } else {
		${log}::warn \
		    "Could not understand \"$meta(Last-Modified)\"\
                         as a date, date parsing error: $mtime"
		set mtime 0
	    }
	} else {
	    ${log}::warn "URL $url has no date!"
	}
	set URLInfo(mtime) $mtime
	${log}::debug "Stored for $url: mtime= $URLInfo(mtime)"
    }

    # Size
    if { [lsearch $keys "size"] >= 0 } {
	set URLInfo(size) $state(totalsize)
	if { $state(totalsize) == 0 } {
	    if { [array names state currentsize] != "" } {
		set URLInfo(size) $state(currentsize)
	    }
	}
	${log}::debug "Stored for $url: size= $URLInfo(size)"
	lappend analysed "size"
    }

    # Final URL
    if { [lsearch $keys "endurl"] >= 0 } {
	set URLInfo(endurl) $state(url)
	${log}::debug "Stored for $url: endurl= $URLInfo(endurl)"
	lappend analysed "endurl"
    }

    return $analysed
}


# ::urlhead::__invoke -- Invoke callbacks.
#
#	Invoke the callbacks that are currently associated to a head
#	information (usually a request.
#
# Arguments:
#	id	The identifier of the cache info, as returned by ::urlhead::get
#	status	Status of the cache info (success or failure).
#	clean	Remove the callbacks once called.
#
# Results:
#	Return the number of callbacks successfully invoked.
#
# Side Effects:
#	None.
proc ::urlhead::__invoke { id status { clean 1 } } {
    variable UH
    variable log

    set varname "::urlhead::URLInfo_${id}"
    upvar \#0 $varname URLInfo

    # Do the callbacks.
    set done 0
    foreach { cb url } $URLInfo(callbacks) {
	${log}::debug "Invoking callback '$cb' for '$url'"
	if { [catch {eval $cb {$url} $URLInfo(id) $status} res] } {
	    ${log}::warn "Error when invoking head callback '$cb'\
                          for '$url': $res"
	} else {
	    incr done
	}
    }

    if { $clean } {
	set URLInfo(callbacks) ""
    }

    return $done
}


# ::urlhead::__done -- Store information and invoke callbacks.
#
#	This command will be called back by massgeturl once a URL head
#	request has been issued (and followed and retried if
#	necessary) and has a result.  Information for the URL is
#	stored and the callbacks are called.
#
# Arguments:
#	cid	The identifier of the massgeturl fetch
#	url	Original URL that was fetched.
#	status	Status of the massgeturl fetch (OK, ERROR or CANCEL)
#	token	Token as returned by http::geturl (or error message)
#
# Results:
#	Return the number of callbacks successfully invoked.
#
# Side Effects:
#	None.
proc ::urlhead::__done { cid url status token } {
    variable UH
    variable log

    set id [find $url]
    if { $id == "" } {
	${log}::warn "$url is not in information cache anymore!"
	return
    }

    set varname "::urlhead::URLInfo_${id}"
    upvar \#0 $varname URLInfo

    # Store everything that we know, and remember we are done.
    if { $status == "OK" } {
	hint $url $token
	set URLInfo(state) "TESTED"
    }

    # Invoke the callbacks and remove them so that they won't be
    # called again.
    __invoke $URLInfo(id) $status

    # Remove the URL information if unsuccessfull.
    if { $status != "OK" } {
	${log}::warn "Failed getting $url: $token"
	remove $URLInfo(id)
    }
}


# ::urlhead::get -- Initiate a URL information store
#
#	Iniatiate a URL information store and see to register a
#	callback that will be invoked once this module has information
#	about the URL or as encountered an error.  If there is a
#	similar request pending for the same URL, the callbacks will
#	be enqueued and called in order later.  Anything not starting
#	with http:/ is considered as a local file.  The callback will
#	be followed by the following arguments: original URL,
#	identifier for the information store and a status (OK or
#	ERROR).  Unsuccesses will lead to an autmoatic removal of the
#	information gathered.
#
# Arguments:
#	url	URL to get information for.
#	cb	Command to call back once done, with arguments as above.
#	force	Should we force refetching of the URL?
#	prio	Priority when talking to massgeturl
#
# Results:
#	Return an identifier for the URL information store.
#
# Side Effects:
#	None.
proc ::urlhead::get { url cb { force 0 } { prio 5 } } {
    variable UH
    variable log

    set id [find $url]
    if { $id != "" } {
	set varname "::urlhead::URLInfo_${id}"
	upvar \#0 $varname URLInfo

	if { $URLInfo(state) == "TESTING" } {
	    ${log}::debug "Request pending on $url, adding callback $cb"
	    lappend URLInfo(callbacks) $cb $url
	    return $URLInfo(id)
	} else {
	    if { $force } {
		# Remove completely the information so that we start
		# from scratch again
		${log}::debug "Forcing regathering of information for $url"
		remove $id
	    } else {
		${log}::debug "Information for $url in cache, direct call"
		lappend URLInfo(callbacks) $cb $url
		__invoke $URLInfo(id) "OK"
		return $URLInfo(id)
	    }
	}
    }

    # Get rid of oldest information from the cache if we have
    # reached the maximum size (and have such a maximum)
    if { $UH(-maxcache) >= 0 && [llength $UH(cache)] > $UH(-maxcache) } {
	${log}::debug "Making space for $url in cache"
	remove [lindex $UH(cache) 0]
    }
    
    # Now start doing the real stuff.
    ${log}::debug "Gathering information for $url"
    set id [incr UH(idgene)]
    set varname "::urlhead::URLInfo_${id}"
    upvar \#0 $varname URLInfo
    set URLInfo(url) $url
    set URLInfo(id) $id
    set URLInfo(ctime) [clock seconds]
    # Include URL in callback for perfect match at caller's side when
    # triggering callback.
    set URLInfo(callbacks) [list $cb $url]
    set URLInfo(state) "TESTING"
    lappend UH(cache) $id
    if { [string match -nocase "http:/*" $url] } {
	${log}::notice "Getting $url to store in head cache"
	
	set url [::massgeturl::urlencode $url]
	set URLInfo(url) $url
	set URLInfo(cid) [::massgeturl::head $url ::urlhead::__done \
			     -priority $prio]
	foreach key [list -timeout -retries -redirects] {
	    ::massgeturl::config $URLInfo(cid) $key $UH($key)
	}
    } else {
	${log}::notice "Getting local file $url into head cache"
	
	if { [file readable $url] } {
	    set URLInfo(mimetype) [::mimetype::guess $url]
	    set URLInfo(mtime) [file mtime $url]
	    set URLInfo(size) [file size $url]
	    set URLInfo(endurl) $url; # We could follow symlinks perhaps here?
	    set URLInfo(state) "TESTED"
	    __invoke $URLInfo(id) "OK"
	} else {
	    set UH(cache) [lrange $UH(cache) 0 end-1]
	    unset URLInfo
	    return ""
	}
    }

    return $URLInfo(id)
}


# ::urlhead::urlinfo -- Get information from cache
#
#	Get all or part of the information about a URL from the cache.
#
# Arguments:
#	id	The identifier of the cache info, as returned by ::urlhead::get
#	key	Key to get value for (mimetype, mtime, size, endurl, url), 
#               empty for all information in a format suitable for array set
#
# Results:
#	Return the value of the key or all values. Empty strings on error.
#
# Side Effects:
#	None.
proc ::urlhead::urlinfo { id { key "" } } {
    variable UH
    variable log

    if { [lsearch $UH(cache) $id] < 0 } {
	${log}::warn "$id does not identify a URL head store in cache!"
	return ""
    }

    set varname "::urlhead::URLInfo_${id}"
    upvar \#0 $varname URLInfo

    if { $key == "" } {
	set res ""
	foreach key $UH(keys) {
	    if { [array names URLInfo $key] == "" } {
		${log}::warn "$URLInfo(url) has no '$key' value (yet?)"
	    }
	    lappend res $key $URLInfo($key)
	}
	return $res
    } else {
	if { [lsearch $UH(keys) $key] < 0 } {
	    ${log}::warn "'$key' is not a valid information for $URLInfo(url)"
	    return ""
	}
	
	if { [array names URLInfo $key] == "" } {
	    ${log}::warn "$URLInfo(url) has no '$key' value (yet?)"
	    return ""
	}
    
	return $URLInfo($key)
    }
}


# ::urlhead::config -- Set/Get settings for all new connections
#
#	This command sets or gets the settings for all new
#	connections, it will not perpetrate on existing pending
#	connections.
#
# Arguments:
#	args	List of -key value or just -key to get value
#
# Results:
#	Return all options, the option requested or set the options
#
# Side Effects:
#	None.
proc ::urlhead::config { args } {
    variable UH
    variable log

    set o [lsort [array names UH "-*"]]

    if { [llength $args] == 0 } {      ;# Return all results
	set result ""
	foreach name $o {
	    lappend result $name $UH($name)
	}
	return $result
    }

    foreach {opt value} $args {        ;# Get onr or set some
	if { [lsearch $o $opt] == -1 } {
	    return -code error "Unknown option $opt, must be: [join $o ,]"
	}
	if { [llength $args] == 1 } {  ;# Get one config value
	    return $UH($opt)
	}
	${log}::debug "Changing $opt from $UH($opt) to $value"
	set UH($opt) $value           ;# Set the config value
    }
}
