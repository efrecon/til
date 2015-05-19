# cache.tcl --
#
#	This modules provides a set of routines to open the content of
#	(local or remote) URLs through a cache system so that the next
#	opening command will possibly be speeded up.
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

package require Tcl 8.2
package require logger
package require diskutil
package require http 2.2
package require mimetype
package require uid
package require uri
package require massgeturl
package require urlhead

package provide urlcache 1.0

# XXX: Adding a "pending" file index would be useful if we want to
# construct some sort of information utility that would pick useful
# information from the cache: showing the status of a file, etc.  Such
# a file beds for restarts against servers that support HTTP1.1 (but
# Tcl does not at the moment).

# XXX: Add a file deletion callback and a "can I delete?" callback.

namespace eval ::urlcache {
    # Initialise the global state
    variable UC
    if {![::info exists UC]} {
	array set UC {
	    idgene        0
	    caches        ""
	    loglevel      warn
	    lasturl       ""
	    lasturl_state ""
	    tmp_prefix    "urlcache"
	    bogus_exts    "mp3"
	    geturl_opts   ""
	}
	variable log [::logger::init [string trimleft [namespace current] ::]]
	${log}::setlevel $UC(loglevel)
	set UC(uids) [::uid::new]
    }

    namespace export loglevel new open urlinfo relax delete cacheinfo \
	flow_monitor delete_unindexed geturl_opts
}


# ::urlcache::loglevel -- Set/Get current log level.
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
proc ::urlcache::loglevel { { loglvl "" } } {
    variable UC
    variable log

    if { $loglvl != "" } {
	if { [catch "${log}::setlevel $loglvl"] == 0 } {
	    set UC(loglevel) $loglvl
	}
    }

    return $UC(loglevel)
}


# ::urlcache::cacheinfo -- Return specific cache information
#
#	This command return specific information about the cache.  The
#	current supported keys are: content (the list of URLs in cache
#	(w/o pending ones)), size (current size of cache (a double)),
#	maxsize (maximum size of cache), dir (directory of cache),
#	idxfname (name of index file in directory).
#
# Arguments:
#	id	Identifier of cache
#	key	Key: content, size, dir, idxfname, maxsize
#
# Results:
#	Return the list of URLs in cache.
#
# Side Effects:
#	None
proc ::urlcache::cacheinfo { id { key "" } } {
    variable UC
    variable log

    set idx [lsearch $UC(caches) $id]
    if { $idx < 0 } {
	${log}::warn "Cache identifier $id is not valid"
	return -code error "Cache identifier invalid"
    }

    # Get to information about this cache.
    set varname "::urlcache::URLCache_$id"
    upvar \#0 $varname Cache

    # Construct an interim array with just enough information
    foreach k [list content size maxsize dir idxfname] {
	set answer($k) $Cache($k)
    }

    # Return the array or one of its specific keys.
    if { $key == "" } {
	return [array get answer]
    } else {
	if { [array names answer $key] != "" } {
	    return $answer($key)
	} else {
	    return ""
	}
    }

    return "" ; # Never reached
}


# ::urlcache::urlinfo -- Return URL specific information
#
#	This command returns specific information about the URL that
#	have been placed in the cache.  The current supported keys
#	are: mtime (the modification time), size (the size of the
#	file), mimetype (the MIME type of the URL, as returned or as
#	guessed) and fname (the name of the localcache file).
#
# Arguments:
#	id	Identifier of cache
#	url	URL to return information for
#	key	Key: mtime, size, mimetype or fname, empty means a list
#               ready for an array set command with all key, value pairs.
#
# Results:
#	Return the value, the list or an empty string on errors.
#
# Side Effects:
#	None
proc ::urlcache::urlinfo { id url { key "" } } {
    variable UC
    variable log

    set idx [lsearch $UC(caches) $id]
    if { $idx < 0 } {
	${log}::warn "Cache identifier $id is not valid"
	return -code error "Cache identifier invalid"
    }

    # Get to information about this cache.
    set varname "::urlcache::URLCache_$id"
    upvar \#0 $varname Cache

    set idx [lsearch $Cache(content) $url]
    if { $idx < 0 } {
	${log}::warn "$url is not in cache $id"
	return ""
    } else {
	set varid [::uid::id $UC(uids) $url]
	set varname "::urlcache::URLInfo_${id}_${varid}"
	upvar \#0 $varname URLInfo

	# Construct an interim array with just enough information
	foreach k [list mtime mimetype size ctime] {
	    set answer($k) $URLInfo($k)
	}
	set answer(fname) [file join $Cache(dir) $URLInfo(fname)]

	# Return the array or one of its specific keys.
	if { $key == "" } {
	    return [array get answer]
	} else {
	    if { [array names answer $key] != "" } {
		return $answer($key)
	    } else {
		return ""
	    }
	}
    }

    return "" ; # Never reached
}


# ::urlcache::__read_index -- Read cache index from disk
#
#	This command reads the cache index file from the disk, if
#	present, and resynchronise the cache to the content of the
#	index file.
#
# Arguments:
#	id	Identifier of cache to read index from
#
# Results:
#	Return the number of files in cache or -1 on errors.
#
# Side Effects:
#	Will empty the current cache.
proc ::urlcache::__read_index { id } {
    variable UC
    variable log

    # Get to information about this cache.
    set varname "::urlcache::URLCache_$id"
    upvar \#0 $varname Cache

    # Attempt to open the cache index description only if it exists
    # and is readable.
    if { [file exists $Cache(idxfname)] && [file readable $Cache(idxfname)] } {
	# Empty current content of cache.
	::urlcache::__remove_urls $id $Cache(content)

	${log}::info "Reading cache index from $Cache(idxfname)"
	# Open and read, line by line, to replace the content of the
	# current cache.
	if { [catch "::open \"$Cache(idxfname)\"" fdes] == 0 } {
	    set Cache(size) 0.0
	    set Cache(content) ""
	    while { ! [eof $fdes] } {
		# Each line contains the following information, and in
		# that order: URL, Time of insertion in the cache,
		# last modification date of the URL, size in bytes of
		# the URL, name of the cache file.
		set line [gets $fdes]

		if { $line != "" } {
		    set url [lindex $line 0]
		    set varid [::uid::id $UC(uids) $url]
		    set varname "::urlcache::URLInfo_${id}_${varid}"
		    upvar \#0 $varname URLInfo
		    set URLInfo(url) $url
		    set URLInfo(ctime) [lindex $line 1]
		    set URLInfo(mtime) [lindex $line 2]
		    set URLInfo(size) [lindex $line 3]
		    set URLInfo(fname) [lindex $line 4]
		    set URLInfo(mimetype) [lindex $line 5]

		    set fullpath [file join $Cache(dir) $URLInfo(fname)]
		    if { ![file exists $fullpath] \
			     || ![file readable $fullpath] } {
			${log}::warn "$fullpath is not accessible, ignoring"
			unset URLInfo
		    } else {
			# Append this URL information to the cache, take the
			# size into account.
			lappend Cache(content) $url
			set Cache(size) [expr {$Cache(size) + $URLInfo(size)}]
		    }
		}
	    }
	    close $fdes
	    return [llength $Cache(content)]
	} else {
	    ${log}::warn "Error when opening index $Cache(idxfname): $fdes"
	    return -1
	}
    } else {
	${log}::notice "Cannot read index at $Cache(idxfname)"
	return -1
    }

    return ; # Never reached.
}


# ::urlcache::__write_index -- Write cache index to disk
#
#	This command write to the disk the index file that fully
#	describes the current state of a cache.
#
# Arguments:
#	id	Identifier of cache to write index for
#
# Results:
#	Return the number of files in cache or -1 on errors.
#
# Side Effects:
#	None.
proc ::urlcache::__write_index { id } {
    variable UC
    variable log

    # Get to information about this cache.
    set varname "::urlcache::URLCache_$id"
    upvar \#0 $varname Cache

    ${log}::info "Writing cache index to $Cache(idxfname)"
    # Open and read, line by line, to replace the content of the
    # current cache.
    if { [catch "::open \"$Cache(idxfname)\" w" fdes] == 0 } {
	foreach url $Cache(content) {
	    set varid [::uid::id $UC(uids) $url]
	    set varname "::urlcache::URLInfo_${id}_${varid}"
	    upvar \#0 $varname URLInfo

	    puts $fdes \
		"\"$url\" $URLInfo(ctime) $URLInfo(mtime) $URLInfo(size)\
                 \"$URLInfo(fname)\" \"$URLInfo(mimetype)\""
	}
	close $fdes
	return [llength $Cache(content)]
    } else {
	${log}::warn "Error when opening index $Cache(idxfname): $fdes"
	return -1
    }

    return ; # Never reached
}


# ::urlcache::__remove_urls -- Remove cache files & info for URLs
#
#	This command will remove the information and associated local
#	files for a number of URLs.
#
# Arguments:
#	id	Identifier of cache to write index for
#	urls	List of URLs to remove information and files for
#
# Results:
#	Return the number of URL information deleted
#
# Side Effects:
#	Will delete local files, if possible.
proc ::urlcache::__remove_urls { id urls } {
    variable UC
    variable log

    # Get to information about this cache.
    set varname "::urlcache::URLCache_$id"
    upvar \#0 $varname Cache

    # Proceed to remove the specified URLs one by one, only if they
    # are part of the cache.
    set nb_removed 0
    foreach url $urls {
	${log}::debug "Removing file & info for $url from cache $Cache(dir)"

	# Check whether this is a URL contained in the cache.
	set idx [lsearch $Cache(content) $url]
	if { $idx >= 0 } {
	    # If it is delete the file on disk and the information
	    # about the URL.
	    set varid [::uid::id $UC(uids) $url]
	    set varname "::urlcache::URLInfo_${id}_${varid}"
	    upvar \#0 $varname URLInfo

	    set fullpath [file join $Cache(dir) $URLInfo(fname)]
	    if { [catch "file delete $fullpath" err] != 0 } {
		${log}::warn "Cannot remove \"$URLInfo(fname)\" from\
		              cache \"$Cache(dir)\"!"
	    }
	    
	    set Cache(size) [expr {$Cache(size) - $URLInfo(size)}]
	    unset URLInfo
	    set Cache(content) [lreplace $Cache(content) $idx $idx]
	    incr nb_removed
	} else {
	    ${log}::warn "Cannot remove \"$url\" from cache \"$Cache(dir)\",\
                          not in cache!"
	}
    }

    return $nb_removed
}


# ::urlcache::__remove_urls -- Remove one cache file & info
#
#	This command will remove the information and associated local
#	files for a given URL.
#
# Arguments:
#	id	Identifier of cache to write index for
#	url	URL to remove information and file for
#
# Results:
#	Return 1 on success, 0 on failure.
#
# Side Effects:
#	Will delete local files, if possible.
proc ::urlcache::__remove_url { id url } {
    return [::urlcache::__remove_urls $id [list $url]]
}


# ::urlcache::__cut_off -- Remove old files when cache is filled
#
#	This command will remove the files and information for the
#	files that are too old and when the cache has reached its
#	maximum size.  This will remove just as enough files for the
#	cache to have a size just under the target size.
#
# Arguments:
#	id	Identifier of cache.
#
# Results:
#	Return number of files removed.
#
# Side Effects:
#	Will delete local files, if possible.
proc ::urlcache::__cut_off { id } {
    variable UC
    variable log

    set varname "::urlcache::URLCache_$id"
    upvar \#0 $varname Cache

    set nb_removed 0
    if { $Cache(size) > $Cache(maxsize) } {
	${log}::notice "Cutting off extraneous size from cache \"$Cache(dir)\""
	
	# First gather information about all URLs into the u_info variable
	set u_info ""
	foreach url $Cache(content) {
	    set varid [::uid::id $UC(uids) $url]
	    set varname "::urlcache::URLInfo_${id}_${varid}"
	    upvar \#0 $varname URLInfo
	    
	    lappend u_info \
		[list "$URLInfo(url)" $URLInfo(ctime) $URLInfo(size)]
	}

	# Sort u_info so that the oldest entries will be first in the list.
	set u_info [lsort -integer -index 1 -increasing $u_info]

	# And construct a list with all the urls that should be removed
	set to_remove ""
	set i 0
	set t_size $Cache(size)
	while { $t_size > $Cache(maxsize) } {
	    set u [lindex $u_info $i]
	    if { $u == "" } {
		break
	    }
	    set url [lindex $u 0]
	    set size [lindex $u 2]
	    set t_size [expr {$t_size - $size}]
	    lappend to_remove "$url"
	    incr i
	}

	# Remove this list of URLs
	set nb_removed [::urlcache::__remove_urls $id $to_remove]
    }

    return $nb_removed
}


# ::urlcache::__flow -- Invoke flow callbacks.
#
#	This command will invoke the flow callbacks.
#
# Arguments:
#	id	Identifier of cache.
#	url	URL
#	op	Operation
#	args	Remaining arguments
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::urlcache::__flow { id url op args } {
    variable UC
    variable log

    # Get to the variable containing information for it.
    set varname "::urlcache::URLCache_$id"
    upvar \#0 $varname Cache

    # Deliver all callbacks in order
    foreach cb $Cache(cbs) {
	if { [catch {eval $cb $id {$url} $op $args} err] } {
	    ${log}::warn "Error when invoking flow callback on $url: $err"
	}
    }
}


# ::urlcache::__invoke -- Invoke a series of callbacks.
#
#	This command will invoke one or more callbacks, catching
#	results in a nice and polite manner.
#
# Arguments:
#	id	Identifier of cache.
#	url	URL
#	cbs	List of callbacks
#	status	Status for the callbacks.
#	txt	Additional argument for the callbacks.
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::urlcache::__invoke { id url cbs status { txt "" } } {
    variable UC
    variable log

    # Deliver all callbacks in order
    ${log}::debug "Delivering [llength $cbs] callbacks for $url"
    foreach cb $cbs {
	${log}::debug "Calling '$cb'... $status"
	if { [catch {eval $cb $id {$url} $status {$txt}} err] } {
	    ${log}::warn "Error when invoking callback on $url: $err"
	}
    }
}


# ::urlcache::__done_finalise -- Finalise URL retrieval and do callbacks
#
#	This command is called when a remote or local URL has been
#	fetched and when all book-keeping for that URL is finalised.
#	It calls the callbacks that have been associated to the URL
#	and remove it from the list of pending URLs.
#
# Arguments:
#	id	Identifier of cache.
#	url	URL fetched
#	status	Status for the callbacks.
#	txt	Additional argument for the callbacks.
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::urlcache::__done_finalize {id url status txt} {
    variable UC
    variable log

    # Get to the variable containing information for it.
    set varname "::urlcache::URLCache_$id"
    upvar \#0 $varname Cache

    set idx [lsearch $Cache(pending) $url]
    if { $idx >= 0 } {
	# Get to the variable containing information for the pending URL.
	set varid [::uid::id $UC(uids) $url]
	set varname "::urlcache::PendingInfo_${id}_${varid}"
	upvar \#0 $varname Pending

	# Deliver successfull callback and remove pending info
	__invoke $id $url $Pending(callbacks) $status $txt
	set Cache(pending) [lreplace $Cache(pending) $idx $idx]
    }
    unset Pending
}


# ::urlcache::__done_success -- Successfull retrieval of URLs
#
#	This command is called when a remote or local URL has
#	successfully been fetched and copied to the local disk.  It
#	stores information into the cache, fixes the cache size and
#	delivers appropriate callbacks.
#
# Arguments:
#	id	Identifier of cache.
#	url	URL fetched
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::urlcache::__done_success {id url} {
    variable UC
    variable log

    # Get to the variable containing information for it.
    set varname "::urlcache::URLCache_$id"
    upvar \#0 $varname Cache

    # Get to the variable containing information for the pending URL.
    # XXX: should we check that it still exists?
    set varid [::uid::id $UC(uids) $url]
    set varname "::urlcache::PendingInfo_${id}_${varid}"
    upvar \#0 $varname Pending

    # Get to information about the URL
    set varname "::urlcache::URLInfo_${id}_${varid}"
    upvar \#0 $varname URLInfo


    # Fill structure with necessary information
    set URLInfo(url) $url
    set URLInfo(ctime) [clock seconds]
    set hid [::urlhead::find $url]
    if { $hid == "" } {
	${log}::warn "Could not get cached information for $url!"
	set URLInfo(mtime) [clock seconds]
	set URLInfo(size) 0
	set URLInfo(mimetype) ""
    } else {
	set URLInfo(mtime) [::urlhead::urlinfo $hid mtime]
	set URLInfo(size) [::urlhead::urlinfo $hid size]
	set URLInfo(mimetype) [::urlhead::urlinfo $hid mimetype]
    }
    set URLInfo(fname) $Pending(fname)

    # Cleanup the HTTP information and append the URL to the cache.
    lappend Cache(content) $url
    set Cache(size) [expr {$Cache(size) + $URLInfo(size)}]

    # Clean cache and write new state.
    ::urlcache::__cut_off $id
    ::urlcache::__write_index $id

    # Deliver successfull callback
    __done_finalize $id $url "OK" $Pending(fullpath)
}


# ::urlcache::__done_failure -- Failed retrieval of URLs
#
#	This command is called when a remote or local URL has failed
#	to be fetched and copied to the local disk.  It removes the
#	local cache file, if possible and delivers appropriate
#	callbacks.
#
# Arguments:
#	id	Identifier of cache.
#	url	URL fetched
#	reason	Description of failure
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::urlcache::__done_failure { id url reason} {
    variable UC
    variable log

    # Get to the variable containing information for it.
    set varname "::urlcache::URLCache_$id"
    upvar \#0 $varname Cache

    set idx [lsearch $Cache(pending) $url]
    if { $idx >= 0 } {
	# Get to the variable containing information for the pending URL.
	set varid [::uid::id $UC(uids) $url]
	set varname "::urlcache::PendingInfo_${id}_${varid}"
	upvar \#0 $varname Pending

	# Remove file from disk
	catch "file delete \"$Pending(fullpath)\""
    }

    # Deliver failure callback.
    __done_finalize $id $url "ERROR" $reason

}

# ::urlcache::__url_done -- Store remote URL information
#
#	This command is called when a remote URL fetching operation
#	has ended.  It analyses the answer (success or failure), store
#	information into the cache, fixes the cache size and delivers
#	appropriate callbacks. Handles redirects correctly.
#
# Arguments:
#	id	Identifier of cache.
#	url	URL fetched
#	token	Token as returned by ::http::geturl
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::urlcache::__url_done { id url cid geturl status token } {
    variable UC
    variable log
    
    # Get to the variable containing information for cache. XXX:
    # Should we check that the cache still exists?
    set varname "::urlcache::URLCache_$id"
    upvar \#0 $varname Cache

    # Get to the variable containing information for the pending URL.
    # XXX: should we check that it still exists?
    set varid [::uid::id $UC(uids) $url]
    set varname "::urlcache::PendingInfo_${id}_${varid}"
    upvar \#0 $varname Pending

    __flow $id $url NET_END
    if { $status == "OK" } {
	# Give a chance to store size of dynamic pages
	::urlhead::hint $url $token [list size]
	::urlcache::__done_success $id $url
    } else {
	::urlcache::__done_failure $id $url $token
    }
}



# ::urlcache::__file_done -- Store local file information
#
#	This command is called when a local file fetching operation
#	has ended.  It analyses the answer (success or failure), store
#	information into the cache, fixes the cache size and deliver
#	appropriate callbacks.
#
# Arguments:
#	id	Identifier of cache.
#	url	URL fetched
#	in_f	File descriptor of local file opened for reading (source!).
#	bytes	Total number of bytes transferred
#	err	Error when copying data between source and destination
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::urlcache::__file_done {id url in_f bytes {err {}} } {
    variable UC
    variable log

    # Get to the variable containing information for cache. XXX:
    # Should we check that the cache still exists?
    set varname "::urlcache::URLCache_$id"
    upvar \#0 $varname Cache

    # Get to the variable containing information for the pending URL.
    # XXX: should we check that it still exists?
    set varid [::uid::id $UC(uids) $url]
    set varname "::urlcache::PendingInfo_${id}_${varid}"
    upvar \#0 $varname Pending
    
    # Close the file descriptor of the files
    catch "close $in_f"
    catch "close $Pending(fdes)"
    ${log}::debug "$url copying completed: $err"

    # If we managed to copy the content of the remote URL, store into
    # cache, otherwise delete local file and deliver error.
    __flow $id $url DISK_END
    if { [string length $err] == 0 } {
	::urlcache::__done_success $id $url
    } else {
	::urlcache::__done_failure $id $url $err
    }
}


# ::urlcache::__url_extension -- Guess a smart extension
#
#	This command will guess an extension in a smart way for the
#	local file.
#
# Arguments:
#	url	URL (local or remote) to extract extension from
#
# Results:
#	Returns an extension, without leading dot
#
# Side Effects:
#	None.
proc ::urlcache::__guess_extension { url } {
    variable UC
    variable log

    # First get the MIME type of the URL and pick up the best
    # extension for that MIME type according to the module.
    set mtype ""
    set id [::urlhead::find $url]
    if { $id != "" } {
	set mtype [::urlhead::urlinfo $id mimetype]
    }
    if { $mtype == "" } {
	${log}::warn "Could not get MIME type of $url, will be a bad guess"
    }
    set mext ""
    if { $mtype != "" } {
	set mext [::mimetype::extension $mtype]
    }
    if { $mext == "" } {
	set mext "tmp"
    }

    # Now see to have in "fname" the raw file name of the file
    # contained in the URL.
    if { [string match -nocase "http:/*" $url] } {
        array set u [::uri::split $url]
        if { $u(path) eq "" } {
	    set lastslash [string last "/" $url]
	    set fname [string range $url $lastslash end]
        } else {
            set fname $u(path)
        }
    } else {
	set fname [file tail $url]
    }

    if { $fname == "" } {
	# No file name, return the MIME good guess extension.
	return $mext
    } else {
	# We have a file name, extract its extension and try to see if
	# it is valid and good to be used.
	set ext [file extension $fname]
	if { [string index $ext 0] == "." } {
	    set ext [string range $ext 1 end]
	}

	# Get the list of extensions for this MIME type.  If we don't
	# have any return the one from the file or the one from the
	# MIME type guessed at the beginning.  If we have some, check
	# that this extension is one of the recognised one; if it is,
	# use it, otherwise, turn back to the default one.
	set mexts [::mimetype::extensions $mtype]
	if { $mexts == "" } {
	    if { $ext != "" } {
		return $ext
	    } else {
		return $mext
	    }
	} else {
	    set idx [lsearch $mexts $ext]
	    if { $idx >= 0 } {
		return $ext
	    } else {
		# Some extensions are widely wrongly associated to
		# strange MIME types, keep these as the origin (.mp3,
		# etc.)
		set idx [lsearch $UC(bogus_exts) $ext]
		if { $idx >= 0 } {
		    return $ext
		} else {
		    return $mext
		}
	    }
	}
    }

    return $mext ; # Never reached
}


# ::urlcache::__url_progress -- Relay progress callbacks.
#
#	This command forwards the progress callbacks issued by the
#	underlying massgeturl module further to callers that wish to
#	know about the data progress flow.
#
# Arguments:
#	id	Identifier of cache.
#	url	URL (local or remote) to store
#	gid	Identifier from massgeturl
#	gurl	URL that was given to massgeturl for fetch (not same as url)
#	furl	URL currently being fetched (massgeturl follows redirects)
#	current	Current number of bytes fetched for furl
#	total	Total number of bytes in furl
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::urlcache::__url_progress { id url gid gurl furl current total } {
    __flow $id $url NET_PROGRESS $furl $current $total
}


# ::urlcache::__url_open -- Store an URL in cache and return local info
#
#	This command will actively store the content of a local file
#	or of a remote HTTP URL into the cache, if possible.  It will
#	handle the maximum size of the cache as appropriate and will
#	deliver a callback when the URL has been read and its data
#	stored in the cache, or when an error has occured.
#
# Arguments:
#	id	Identifier of cache.
#	url	URL (local or remote) to store
#	opencb	Callback to call on success/failure
#	prio	Priority when talking to massgeturl
#
# Results:
#	Returns 1 if the fetching process could be initiated, 0 otherwise.
#
# Side Effects:
#	None.
proc ::urlcache::__url_open { id url opencb prio } {
    variable UC
    variable log

    # Get to the variable containing information for it.
    set varname "::urlcache::URLCache_$id"
    upvar \#0 $varname Cache

    # Check whether we are already fetching this URL into the cache.
    # In that case, simply append the callback to the list of
    # callbacks to call at end.
    set idx [lsearch $Cache(pending) $url]
    if { $idx >= 0 } {
	set varid [::uid::id $UC(uids) $url]
	set varname "::urlcache::PendingInfo_${id}_${varid}"
	upvar \#0 $varname Pending

	${log}::debug "Already downloading $url, adding '$opencb' to callbacks"
	lappend Pending(callbacks) $opencb
	if { $Pending(cid) != "" } {
	    ::massgeturl::config $Pending(cid) -priority $prio
	}
	return 1
    }

    # Decide upon an appropriate extension for the local cache file
    set ext [::urlcache::__guess_extension $url]

    # Now open the local cache file for writing.
    set fname [::diskutil::temporary_filename $UC(tmp_prefix) $ext]
    set fullpath [file join $Cache(dir) $fname]
    ${log}::debug "Storing content of $url into $fullpath"

    if { [string match -nocase "http:/*" $url] } {
	# Decide upon the URL to really get (the HEAD request may have
	# followed redirects already, so this is an optimisation not
	# to follow redirects twice).
	set geturl $url
	set hid [::urlhead::find $geturl]
	if { $hid != "" } {
	    set geturl [::urlhead::urlinfo $hid endurl]
	    if { $geturl == "" } {
		set geturl $url
	    }
	}

	# And fetch
	set cmd [list ::massgeturl::infile $geturl $fullpath \
		     [list ::urlcache::__url_done $id $url] \
		     -progress [list ::urlcache::__url_progress $id $url] \
		     -priority $prio]
	foreach opt $Cache(geturl_opts) {
	    lappend cmd $opt
	}
	if { [catch {eval $cmd} cid] == 0 } {
	    set varid [::uid::id $UC(uids) $url]
	    set varname "::urlcache::PendingInfo_${id}_${varid}"
	    upvar \#0 $varname Pending

	    set Pending(type) URL
	    set Pending(cache) $id
	    set Pending(url) $url
	    set Pending(fullpath) $fullpath
	    set Pending(fname) $fname
	    set Pending(fdes) ""
	    ${log}::debug "Initialising callbacks list to '$opencb'"
	    set Pending(callbacks) [list $opencb]
	    set Pending(cid) $cid

	    lappend Cache(pending) $url
	    if { $hid != "" } {
		__flow $id $url NET_START \
		    $geturl [::urlhead::urlinfo $hid size]
	    } else {
		__flow $id $url NET_START $url -1
	    }
	} else {
	    __flow $id $url NET_ERROR
	    ${log}::warn "Cannot get content of remote URL $url: $cid"
	    return 0
	}
    } else {
	if { [catch {::open $fullpath w} fdes] } {
	    ${log}::warn "Cannot create cache file \"$fullpath\""
	    return 0
	}
	if { [catch {::open $url} src_fdes] == 0 } {
	    set varid [::uid::id $UC(uids) $url]
	    set varname "::urlcache::PendingInfo_${id}_${varid}"
	    upvar \#0 $varname Pending

	    set Pending(type) FILE
	    set Pending(cache) $id
	    set Pending(url) $url
	    set Pending(fullpath) $fullpath
	    set Pending(fname) $fname
	    set Pending(fdes) $fdes
	    ${log}::debug "Initialising callbacks list to '$opencb'"
	    set Pending(callbacks) [list $opencb]
	    set Pending(cid) ""

	    lappend Cache(pending) $url

	    __flow $id $url DISK_START $fullpath [file size $fullpath]
	    fconfigure $fdes -encoding binary -translation binary
	    fconfigure $src_fdes -encoding binary -translation binary
	    fcopy $src_fdes $fdes \
		-command [list ::urlcache::__file_done $id $url $src_fdes]
	} else {
	    close $fdes
	    ${log}::warn "Could not open file $url for reading"
	    return 0
	}
    }

    return 1
}


# ::urlcache::find -- Find a cache
#
#	Find if a cache object already exists for a given directory
#
# Arguments:
#	dir	Top directory of cache (available in rw!)
#
# Results:
#	Return an identifier for the cache object, or an empty string
#
# Side Effects:
#	None.
proc ::urlcache::find { dir } {
    variable UC
    variable log

    if { ! [file isdirectory $dir] } {
	${log}::warn "\"$dir\" is not a directory!"
	return ""
    }
    if { ! [file readable $dir] } {
	${log}::warn "Cannot read from \"$dir\"!"
	return ""
    }

    set absdir [::diskutil::absolute_path $dir]
    foreach id $UC(caches) {
	# Get to the variable containing information for it.
	set varname "::urlcache::URLCache_$id"
	upvar \#0 $varname Cache

	if { $Cache(dir) == $absdir } {
	    return $id
	}
    }

    return ""
}



# ::urlcache::new -- (Re)create a new cache
#
#	Create a new cache, using a specific directory on disk.  The
#	content of previous cache creation within that very location
#	will be reused if appropriate.
#
# Arguments:
#	dir	Top directory of cache (available in rw!)
#	maxsize	Maximum size of cache in KB
#	idxfname	Name of index file for cache.
#
# Results:
#	Return an identifier that will be used in all further call to
#	this module, an empty string in case of error (for example
#	when the directory cannot be created or opened).
#
# Side Effects:
#	None.
proc ::urlcache::new { dir { maxsize 1024 } { idxfname "cache.idx" } } {
    variable UC
    variable log

    # Check existence of directory and whether we are able to
    # read/write files into it.  Create this directory if necessary.
    if { ! [file exists $dir] } {
	${log}::notice "Creating directory \"$dir\""
	if { [catch "file mkdir $dir" err] != 0 } {
	    ${log}::warn "Cannot create directory \"$dir\": $err"
	    return ""
	}
    }
    if { ! [file isdirectory $dir] } {
	${log}::warn "\"$dir\" is not a directory!"
	return ""
    }
    if { ! [file readable $dir] } {
	${log}::warn "Cannot read from \"$dir\"!"
	return ""
    }
    if { ! [file writable $dir] } {
	${log}::warn "Cannot write to \"$dir\"!"
	return ""
    }

    # Now, create or reuse cache object
    set id [::urlcache::find $dir]
    if { $id != "" } {
	set varname "::urlcache::URLCache_$id"
	upvar \#0 $varname Cache
	set Cache(maxsize) [expr {double($maxsize) * 1024}]
	if { [::urlcache::__cut_off $id] > 0 } {
	    ${log}::notice \
		"New instantiated cache smaller than previous, resynchronising"
	    ::urlcache::__write_index $id
	}
	return $id
    }

    # Generate an identifier from the absolute path of the directory.
    # A previous version was using an incremented counter, but that
    # would not perdure over several runs if the information was
    # stored outside of this module (for example on disk).
    set absdir [::diskutil::absolute_path $dir]
    set id [::uid::id $UC(uids) $absdir]

    set varname "::urlcache::URLCache_$id"
    upvar \#0 $varname Cache

    set Cache(id) $id
    set Cache(dir) $absdir
    set Cache(maxsize) [expr {double($maxsize) * 1024}]
    set Cache(idxfname) [file join $Cache(dir) [file tail $idxfname]]
    set Cache(pending) ""
    set Cache(size) 0.0
    set Cache(content) ""
    set Cache(relaxed_types) ""
    set Cache(idgene) 0
    set Cache(cbs) ""
    set Cache(geturl_opts) $UC(geturl_opts)

    lappend UC(caches) $id

    # See to get infinite usage of urlhead
    ::urlhead::config -maxcache -1

    # Resynchronise with current content
    if { [::urlcache::__read_index $id] >= 0 } {
	if { [::urlcache::__cut_off $id] > 0 } {
	    ${log}::notice \
		"New instantiated cache smaller than previous, resynchronising"
	    ::urlcache::__write_index $id
	}
    }

    return $id
}


# ::urlcache::__url_open_cb -- Local callback called when in blocking mode
#
#	This command is called when fetching through the cache is made
#	in blocking mode.  It mainly aims at unblocking the caller
#	procedure by signalising through the Cache struture.
#
# Arguments:
#	id	Identifier of cache.
#	url	URL (local or remote) to store
#	status	Status of cache storage: OK or ERROR
#
# Results:
#	None
#
# Side Effects:
#	None.
proc ::urlcache::__url_open_cb { sid id url status {fname "" } } {
    variable UC
    variable log

    ${log}::debug "$url fetching process ended: $status -> $fname"

    # Get to the variable containing information for it.
    set varname "::urlcache::URLCache_$id"
    upvar \#0 $varname Cache

    set varname "::urlcache::Sync_${id}_${sid}"
    upvar \#0 $varname Sync

    if { $status == "OK" && [string length $fname] != 0 } {
	set Sync(fname) $fname
    }
    set Sync(finished) 1
}


# ::urlcache::__url_find -- Find URL in cache
#
#	This command finds if a URL is already present in a cache.  It
#	handles appropriately URL encodings (and should perhaps handle
#	IP vs. host names).
#
# Arguments:
#	id	Identifier of cache.
#	url	URL to find in cache.
#	url_p	Pointer to variable that will contain URL in cache on success
#
# Results:
#	Index of URL in content list, -1 if not found, error for an
#       unknown cache.
#
# Side Effects:
#	None.
proc ::urlcache::__url_find { id url { url_p "" } } {
    variable UC
    variable log

    # Check that this is one of our caches.
    set idx [lsearch $UC(caches) $id]
    if { $idx < 0 } {
	${log}::warn "Cache identifier $id is not valid"
	return -code error "Cache identifier invalid"
    }

    # Get to the variable containing information for it.
    set varname "::urlcache::URLCache_$id"
    upvar \#0 $varname Cache

    set idx 0
    set encurl [::massgeturl::urlencode $url]
    foreach u $Cache(content) {
	if { [::massgeturl::urlencode $u] eq $encurl } {
	    if { $url_p ne "" } {
		upvar $url_p outurl
		set outurl $u
	    }
	    return $idx
	}
	incr idx
    }

    return -1
}


# ::urlcache::__head_done -- Core URL fetching routine
#
#	This command is called as a callback of the urlhead package
#	whenever a URL has been validated and its information is
#	known.  This command decides upon the course of action:
#	delivering a callback pointing at the (old) content of the URL
#	and/or fetching the (new) content of the URL.
#
# Arguments:
#	id	Identifier of cache.
#	sid	Identifier of synchronisation, -1 if none requested
#	open_cb	Command to callback when the content is accessible
#	prio	Priority to use against massgeturl
#	hid	Identifier of the URL head operation
#	status	Status result of the URL head operation
#
# Results:
#	None
#
# Side Effects:
#	Will possibly fetch the content of the URL to the local disk.
proc ::urlcache::__head_done { id sid open_cb prio url hid status } {
    variable UC
    variable log

    # Check that this is one of our caches.
    set idx [lsearch $UC(caches) $id]
    if { $idx < 0 } {
	${log}::warn "Cache identifier $id is not valid"
	return -code error "Cache identifier invalid"
    }

    # Get to the variable containing information for it.
    set varname "::urlcache::URLCache_$id"
    upvar \#0 $varname Cache

    set has_copy ""

    if { $status == "OK" } {
	set idx [__url_find $id $url urlc]
	if { $idx >= 0 } {
	    # We successfully got the time for the URL, check against our
	    # cached time and reread from remote or return local.
	    set varid [::uid::id $UC(uids) $urlc]
	    set varname "::urlcache::URLInfo_${id}_${varid}"
	    upvar \#0 $varname URLInfo
	    set mtime [::urlhead::urlinfo $hid mtime]
	    if { $mtime <= $URLInfo(mtime) && $mtime != 0 } {
		set has_copy [file join $Cache(dir) $URLInfo(fname)]
	    } else {
		::urlcache::__remove_url $id $urlc
		::urlcache::__write_index $id
	    }
	}
    } else {
	# Error when accessing the head of the URL, either we return
	# the local copy anyhow if the type is relaxed, either we
	# deliver an error.
	set idx [__url_find $id $url urlc]
	if { $idx >= 0 } {
	    set varid [::uid::id $UC(uids) $urlc]
	    set varname "::urlcache::URLInfo_${id}_${varid}"
	    upvar \#0 $varname URLInfo
	    
	    set relaxit 0
	    foreach ptn $Cache(relaxed_types) {
		if { [string match $ptn $URLInfo(mimetype)] } {
		    set relaxit 1
		}
	    }
	    if { $relaxit } {
		set has_copy [file join $Cache(dir) $URLInfo(fname)]
	    } else {
		# Deliver an error through the callback or simply by
		# mediating back to the synchronisation structure.
		${log}::warn "Could not get the modification date of $url,\
                              which is not a relaxed type"
		if { $sid >= 0 } {
		    set varname "::urlcache::Sync_${id}_${sid}"
		    upvar \#0 $varname Sync
		    set Sync(finished) 1
		} else {
		    __invoke $id $url [list $open_cb] "ERROR" \
			"Cannot access modification time for $url"
		}
		return -code error "Modification time cannot be accessed!"
	    }
	} else {
	    # Deliver an error through the callback or simply by
	    # mediating back to the synchronisation structure.
	    ${log}::warn "Could not get the modification date of $url,\
                              which is not a relaxed type"
	    if { $sid >= 0 } {
		set varname "::urlcache::Sync_${id}_${sid}"
		upvar \#0 $varname Sync
		set Sync(finished) 1
	    } else {
		__invoke $id $url [list $open_cb] "ERROR" \
		    "Cannot access modification time for $url"
	    }
	    return -code error "Modification time cannot be accessed!"
	}
    }

    if { $has_copy == "" } {
	# Call ::urlcache::__url_open with an appropriate callback to
	# handle both the cases of an open_cb style (non-blocking) and
	# a blocking style of calling this routine.
	if { $open_cb == "" } {
	    if { [::urlcache::__url_open $id $url \
		      [list ::urlcache::__url_open_cb $sid] $prio] } {
	    } else {
		${log}::warn "Could not open $url"
		set Sync(finished) 1
	    }
	} else {
	    if { ! [::urlcache::__url_open $id $url $open_cb $prio] } {
		${log}::warn "Could not open URL"
	    }
	}
    } else {
	__flow $id $url FROMCACHE "$has_copy"
	if { $sid >= 0 } {
	    set varname "::urlcache::Sync_${id}_${sid}"
	    upvar \#0 $varname Sync
	    set Sync(fname) $has_copy
	    set Sync(finished) 1
	} else {
	    # Deliver successfull callback
	    __invoke $id $url [list $open_cb] "OK" $has_copy
	}
    }
}


# ::urlcache::geturl_opts -- Set massgeturl options
#
#	This command sets the options that will be passed further to
#	the massgeturl module when fetching URL from the Internet.
#
# Arguments:
#	id	Identifier of cache.
#	args	argument list.
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::urlcache::geturl_opts { id args } {
    variable UC
    variable log

    # Check that this is one of our caches.
    set idx [lsearch $UC(caches) $id]
    if { $idx < 0 } {
	${log}::warn "Cache identifier $id is not valid"
	return -code error "Cache identifier invalid"
    }

    # Get to the variable containing information for it.
    set varname "::urlcache::URLCache_$id"
    upvar \#0 $varname Cache

    # XXX: This is hasardous, we should perhaps check that the options
    # are valid options.  This can be done through asking massgeturl
    # what its options are by calling ::massgeturl::defaults without
    # arguments
    set Cache(geturl_opts) $args
}


# ::urlcache::open -- Open a local/remote URL via the cache.
#
#	This command will open a remote URL or a local file via the
#	cache.  Both types of URLs will be locally stored in the cache
#	whenever possible, so that next access will attempt to return
#	the cached value.  Modification times for files and URLs are
#	used when deciding if the cached value should be returned.
#	Called without a callback, this command will block the caller
#	until the remote URL or the local file has been moved into the
#	cache successfully or not and will then return the name of the
#	local (cache) file containing data, or an empty string.  The
#	callback takes 3 or 4 more arguments: the identifier of the
#	cache, the URL that was fetched, a status (OK or ERROR) and a
#	local (cache) file name when the status is OK.
#
# Arguments:
#	id	Identifier of cache.
#	url	URL (local or remote) to store
#	open_cb	Command to callback on success/failure
#	prio	Priority for URL fetching when using massgeturl
#
# Results:
#	None if called in non-blocking mode, the name of the file
#	containing cached data or an empty string when called in
#	blocking mode.
#
# Side Effects:
#	This command will create local files in the directory used by
#	the cache and will also remove older files from that directory
#	if necessary.
proc ::urlcache::open { id url { open_cb "" } { prio 5 } } {
    variable UC
    variable log

    # Check that this is one of our caches.
    set idx [lsearch $UC(caches) $id]
    if { $idx < 0 } {
	${log}::warn "Cache identifier $id is not valid"
	return -code error "Cache identifier invalid"
    }

    # Get to the variable containing information for it.
    set varname "::urlcache::URLCache_$id"
    upvar \#0 $varname Cache

    # Check if we should fetch the URL, either since it is not in the
    # cache, either since it is accessible at a newer version.
    set idx [__url_find $id $url]
    set force [expr {$idx >= 0}]
    
    if { $open_cb == "" } {
	set sid [incr Cache(idgene)]
	set varname "::urlcache::Sync_${id}_${sid}"
	upvar \#0 $varname Sync

	set Sync(sid) $sid
	set Sync(url) $url
	set Sync(cid) $id;  # Cache identifier
	set Sync(finished) 0
	set Sync(fname) ""
    } else {
	set sid -1
    }

    if { 0 } {
	set pending [info vars ::urlcache::PendingInfo*]
	set reqsize [expr int (1.1 * [llength $pending])]
	if { [::urlhead::config -maxcache] < $reqsize } {
	    ::urlhead::config -maxcache $reqsize
	}
    }
    
    __flow $id $url INIT
    set hid [::urlhead::get $url \
		 [list ::urlcache::__head_done $id $sid $open_cb $prio] \
		 $force [expr {$prio + 1}]]
    if { $sid >= 0 } {
	if { ! $Sync(finished) } {
	    vwait ::urlcache::Sync_${id}_${sid}(finished)
	}

	set fname $Sync(fname)
	unset Sync
	return $fname
    }

    return ""
}


# ::urlcache::flow_monitor -- Monitor the flow of fetching operations
#
#	This command will registers callbacks that will be called to
#	monitor the flow of operations: downloading start, etc.  The
#	commands registered will be called with the following
#	arguments: the identifier of the cache, the url being fetched,
#	an operation and some additional arguments describing the
#	operation further.  The operations and their arguments are:
#       INIT        - initialisation, no arguments
#       FROMCACHE   - Fetch from cache, full path to file as argument
#       DISK_START  - Copying from disk into cache, full path to source
#                     and size of file as arguments.
#       DISK_END    - Done with disk copy.
#       NET_START   - Fetch from the Internet, path to URL being fetched
#                     (redirects: might be different) and estimated size
#                     (-1 if unknown) as arguments.
#       NET_ERROR   - Error on network fetching
#       NET_PROGRESS- Progress on Internet fetch.  Arguments are the url
#                     really being fetch, the current number of bytes and
#                     the total number of bytes to be fetched.  The total
#                     could be -1, there could be several "real" URLs
#                     because of redirects.
#       NET_END     - Done with network fetch.
#
# Arguments:
#	id	Identifier of cache.
#	cb	Callback for monitoring
#
# Results:
#	None
#
# Side Effects:
#	None
proc ::urlcache::flow_monitor { id cb } {
    variable UC
    variable log

    # Check that this is one of our caches.
    set idx [lsearch $UC(caches) $id]
    if { $idx < 0 } {
	${log}::warn "Cache identifier $id is not valid"
	return -code error "Cache identifier invalid"
    }

    # Get to the variable containing information for it.
    set varname "::urlcache::URLCache_$id"
    upvar \#0 $varname Cache

    if { $cb != "" } {
	lappend Cache(cbs) $cb
    }

    return
}


# ::urlcache::relax -- Relax integrity for some MIME types
#
#	This command will sees to relax the integrity for the MIME
#	types that match the patterns passed as arguments.  Relaxing
#	means that when the remote URL is not accessible for any
#	reason and when there is a local copy that is left, the cache
#	will return the local copy even if it cannot check the
#	modification date of the remote URL.  This command can be
#	called several times, and will add MIME types patterns to an
#	internal list.
#
# Arguments:
#	id	Identifier of cache.
#	mt_ptn	MIME type pattern to relax on, string match-like (empty=query)
#
# Results:
#	The current list of relaxed types.
#
# Side Effects:
#	Explained above.
proc ::urlcache::relax { id { mt_ptn "" } } {
    variable UC
    variable log

    # Check that this is one of our caches.
    set idx [lsearch $UC(caches) $id]
    if { $idx < 0 } {
	${log}::warn "Cache identifier $id is not valid"
	return -code error "Cache identifier invalid"
    }

    # Get to the variable containing information for it.
    set varname "::urlcache::URLCache_$id"
    upvar \#0 $varname Cache

    if { $mt_ptn != "" } {
	lappend Cache(relaxed_types) $mt_ptn
    }

    return $Cache(relaxed_types)
}


# ::urlcache::delete_unindexed -- Clean non-indexed files
#
#	This command will delete all files that match a pattern from a
#	cache directory if they are not indexed from within the cache.
#	This command is usefull to clean files which download had
#	started but interrupted before being indexed.
#
# Arguments:
#	id	Identifier of cache.
#	ptn	pattern
#
# Results:
#	Return the number of files that were removed
#
# Side Effects:
#	This command will actively delete files!
proc ::urlcache::delete_unindexed { id { ptn * } } {
    variable UC
    variable log

    # Check that this is one of our caches.
    set idx [lsearch $UC(caches) $id]
    if { $idx < 0 } {
	${log}::warn "Cache identifier $id is not valid"
	return -code error "Cache identifier invalid"
    }

    # Get to the variable containing information for it.
    set varname "::urlcache::URLCache_$id"
    upvar \#0 $varname Cache

    set removed 0
    set allfiles [glob -nocomplain -tails -directory $Cache(dir) $ptn]
    foreach fname $allfiles {
	set remove 1
	if { $fname == [file tail $Cache(idxfname)] } {
	    set remove 0
	}
	foreach url $Cache(content) {
	    set varid [::uid::id $UC(uids) $url]
	    set varname "::urlcache::URLInfo_${id}_${varid}"
	    upvar \#0 $varname URLInfo

	    if { $fname == $URLInfo(fname) } {
		set remove 0
	    }
	}

	if { $remove } {
	    ${log}::debug "Removing $fname from $Cache(dir)"
	    if { [catch {file delete [file join $Cache(dir) $fname]} err] } {
		${log}::warn "Could not remove $fname from $Cache(dir): $err"
	    } else {
		incr removed
	    }
	}
    }

    ${log}::info \
	"Cleaned $removed non-indexed files matching '$ptn' in $Cache(dir)"
    
    return $removed
}


# ::urlcache::delete -- Delete a cache
#
#	This command will delete a cache and (possibly) all
#	information associated to it.  The default is to keep the
#	index and the cache files on disk so that later instatiation a
#	cache in that same directory will be able to catch up with the
#	same state.  However, a true cleaning of the directory can be
#	achieved, in which case only an empty index file is left.
#
# Arguments:
#	id	Identifier of cache.
#	cleanup	Should we completely remove all cached files?
#
# Results:
#	None.
#
# Side Effects:
#	This command will attempt to remove local cache files if a
#	full cleanup is requested.
proc ::urlcache::delete { id { cleanup 0 } } {
    variable UC
    variable log

    # Check that this is one of our caches.
    set idx [lsearch $UC(caches) $id]
    if { $idx < 0 } {
	${log}::warn "Cache identifier $id is not valid"
	return -code error "Cache identifier invalid"
    }

    # Get to the variable containing information for it.
    set varname "::urlcache::URLCache_$id"
    upvar \#0 $varname Cache

    if { $cleanup } {
	# True cleanup, destroy all files and write an empty index
	::urlcache::__remove_urls $id $Cache(content)
	::urlcache::__write_index $id
    } else {
	# Soft cleanup, clear all local data, but keep the files.
	foreach url $Cache(content) {
	    ${log}::debug "Soft removing of information for $url"
	    set varid [::uid::id $UC(uids) $url]
	    set varname "::urlcache::URLInfo_${id}_${varid}"
	    upvar \#0 $varname URLInfo
	
	    unset URLInfo
	}
    }

    # Arrange so that we do not know anything about the cache anymore
    unset Cache
    set UC(caches) [lreplace $UC(caches) $idx $idx]
}
