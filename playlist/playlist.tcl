# playlist.tcl --
#
#	This module provides a playlist mechanism.
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

package require Tcl 8.2
package require logger
package require diskutil

package require snack

package provide playlist 1.0


namespace eval ::playlist {
    # Initialise the global state
    variable PLS
    if {![::info exists PLS]} {
	array set PLS {
	    idgene        0
	    loglevel      warn
	    playlists     ""
	}
	variable log [::logger::init playlist]
	${log}::setlevel $PLS(loglevel)
    }

    namespace export new add remove fileinfo play stop pause next previous
}


# ::playlist::loglevel -- Set/Get current log level.
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
proc ::playlist::loglevel { { loglvl "" } } {
    variable PLS
    variable log

    if { $loglvl != "" } {
	if { [catch "${log}::setlevel $loglvl"] == 0 } {
	    set PLS(loglevel) $loglvl
	}
    }

    return $PLS(loglevel)
}


# ::playlist::__do_cbs -- Call relevant callbacks
#
#	Call the callbacks that match the operation passed as parameter.
#
# Arguments:
#	id	Identifier of play list, as returned by ::playlist::new
#	op	Name of operation
#	fid	Identifier of file in play list.
#	args	Additional arguments
#
# Results:
#	Return the number of callbacks successfully called, -1 on failure.
#
# Side Effects:
#	None.
proc ::playlist::__do_cbs { id op fid args } {
    variable PLS
    variable log

    set idx [lsearch $PLS(playlists) $id]
    if { $idx < 0 } {
	${log}::warn "$id is not a valid playlist"
	return -1
    }
    
    set varname "::playlist::pls_$id"
    upvar \#0 $varname PlayList

    set called 0
    foreach { op_ptn cmd } $PlayList(callbacks) {
	if { [string match $op_ptn $op] } {
	    if { [catch {eval $cmd $id $op $fid $args} err] } {
		${log}::warn \
		    "Error while calling $op callback, executin $cmd: $err"
	    } else {
		incr called
	    }
	}
    }

    return $called
}


# ::playlist::remove -- Remove files from a playlist
#
#	Remove one or more files from a play list.
#
# Arguments:
#	id	Identifier of play list, as returned by ::playlist::new
#	tagptn	Tag pattern
#	delsnd	Should we delete the snack sound as well?
#
# Results:
#	Return a list of identifiers (aka unique tags) for the files
#	that were removed
#
# Side Effects:
#	None.
proc ::playlist::remove { id tagptn { delsnd on } } {
    variable PLS
    variable log

    set idx [lsearch $PLS(playlists) $id]
    if { $idx < 0 } {
	${log}::warn "$id is not a valid playlist"
	return ""
    }
    
    set varname "::playlist::pls_$id"
    upvar \#0 $varname PlayList

    set to_remove [::playlist::get $id $tagptn]
    ${log}::debug "Removing files $to_remove from playlist $id..."
    set removed ""
    foreach fid $to_remove {
	set idx [lsearch $PlayList(files) $fid]
	if { $idx < 0 } {
	    ${log}::warn "$fid is not an identifier in play list $id!"
	} else {
	    __do_cbs $id REMOVE $fid
	    set PlayList(files) [lreplace $PlayList(files) $idx $idx]
	    set varname "::playlist::files_${id}_${fid}"
	    upvar \#0 $varname File
	    lappend removed $File(id)
	    
	    if { $File(sound) != "" && [string is true $delsnd] } {
		# If we have requested to destroy the sounds as well,
		# we have to take special care of the sound of the
		# file currently being played, if any.  We move it to
		# a special variable, which will be inquired at a
		# later time when we know that the sound is not being
		# played anymore.
		if { $File(id) != $PlayList(current) } {
		    ${log}::debug "Destroying $File(sound) / $File(fname)"
		    $File(sound) destroy
		} else {
		    ${log}::debug "Scheduling later destroying of $File(sound)"
		    lappend PlayList(destroy_onnext) $File(sound)
		}
	    }
	    unset File
	}
    }
    
    return $removed
}


# ::playlist::fileinfo -- Get file information
#
#	Get and return information for a file contained in a play
#	list.  This command returns a list ready for an array set
#	command, containing the following keys: fname, tags, id,
#	snd_start and snd_end; or the value of one of these recognised
#	keys.
#
# Arguments:
#	id	Identifier of play list, as returned by ::playlist::new
#	fid	Identifier of file in play list.
#	key	Key of the information to return, empty for array set list
#
# Results:
#	Return the value, the list or an empty string on errors.
#
# Side Effects:
#	None.
proc ::playlist::fileinfo { id fid { key "" } } {
    variable PLS
    variable log

    set idx [lsearch $PLS(playlists) $id]
    if { $idx < 0 } {
	${log}::warn "$id is not a valid playlist"
	return ""
    }
    
    set varname "::playlist::pls_$id"
    upvar \#0 $varname PlayList

    foreach i_fid $PlayList(files) {
	if { $i_fid == $fid } {
	    set varname "::playlist::files_${id}_${fid}"
	    upvar \#0 $varname File

	    foreach k [list fname tags id snd_start snd_end] {
		set answer($k) $File($k)
	    }

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
    }

    return ""
}


# ::playlist::get -- Get file(s) by its/their tags
#
#	Return the identifiers of the files that match a particular
#	tag pattern.
#
# Arguments:
#	id	Identifier of play list, as returned by ::playlist::new
#	tagptn	Tag pattern
#
# Results:
#	Return a list of identifiers (aka unique tags) for the files
#	matching the tag pattern.  The list might be empty.
#
# Side Effects:
#	None.
proc ::playlist::get { id tagptn } {
    variable PLS
    variable log

    set idx [lsearch $PLS(playlists) $id]
    if { $idx < 0 } {
	${log}::warn "$id is not a valid playlist"
	return ""
    }
    
    set ids ""

    set varname "::playlist::pls_$id"
    upvar \#0 $varname PlayList

    foreach fid $PlayList(files) {
	set varname "::playlist::files_${id}_${fid}"
	upvar \#0 $varname File
	
	foreach tag $File(tags) {
	    if { [string match $tagptn $tag] } {
		lappend ids $File(id)
	    }
	}
    }

    set ids [lsort -unique $ids]
    ${log}::debug "Files matching $tagptn in playlist $id: $ids"

    return $ids
}


# ::playlist::position -- Get position of file
#
#	Return the position (index) of a file within the playlist.
#
# Arguments:
#	id	Identifier of play list, as returned by ::playlist::new
#	fid	Identifier of file (empty is current)
#
# Results:
#	Return the position of the file or an empty string if it is
#	not in the playlist
#
# Side Effects:
#	None.
proc ::playlist::position { id { fid "" } } {
    variable PLS
    variable log

    set idx [lsearch $PLS(playlists) $id]
    if { $idx < 0 } {
	${log}::warn "$id is not a valid playlist"
	return ""
    }

    set varname "::playlist::pls_$id"
    upvar \#0 $varname PlayList

    if { $fid == "" } {
	set fid $PlayList(current)
    }
    
    set pos 0
    foreach file_id $PlayList(files) {
	if { $file_id == $fid } {
	    return $pos
	}
	incr pos
    }

    return ""
}


# ::playlist::add -- Add a file
#
#	Insert a file into a play list.  This command will
#	automatically associate two tags to the file that is added to
#	the play list: one unique identifier, which is returned, and
#	the name of the file.
#
# Arguments:
#	id	Identifier of play list, as returned by ::playlist::new
#	fname	Full path to file to add to playlist
#	pos	Position in play list, (defaults to "end", append to playlist)
#	tags	List of tags that will be associated to the file
#	start	Start time within file, milliseconds from beginning.
#	end	End time within file, milliseconds from beginning or "end"
#
# Results:
#	Return the automatically generated (and unique) tag associated
#	to the file or an empty string on
#	errors.
#
# Side Effects:
#	None.
proc ::playlist::add { id fname {pos end} {tags {}} {start 0} {end end} } {
    variable PLS
    variable log
    
    ${log}::info \
	"Adding $fname to playlist $id at position $pos, tagging it with $tags"
    
    set idx [lsearch $PLS(playlists) $id]
    if { $idx < 0 } {
	${log}::warn "$id is not a valid playlist"
	return ""
    }

    if { ![file readable $fname] } {
	${log}::warn "Cannot access file $fname"
	return ""
    }

    set varname "::playlist::pls_$id"
    upvar \#0 $varname PlayList

    set fid $PlayList(idgene)
    set varname "::playlist::files_${id}_${fid}"
    upvar \#0 $varname File
    incr PlayList(idgene)

    set File(id) $fid
    set File(playlist) $id
    set File(fname) [::diskutil::absolute_path $fname]
    set File(sound) ""
    set File(tags) [concat $fid "$fname" $tags]
    set File(snd_start) $start
    set File(snd_end) $end
    if { ![string is double $File(snd_start)] || $File(snd_start) < 0 } {
	set File(snd_start) 0
    }
    if { ![string is double $File(snd_end)] || $File(snd_end) < 0 } {
	set File(snd_end) end
    }
    
    if { $pos == "" } {
	set pos end
    }
    set PlayList(files) [linsert $PlayList(files) $pos $fid]
    __do_cbs $id ADD $fid

    return $fid
}


# ::playlist::__cleanup -- Cleanup Orphan Sounds
#
#	This command is called whenever a new file is being started to
#	be played.  It removes all sounds that were marked as deleted
#	but could not be immediately be deleted since they were being
#	played at the time of removal.
#
# Arguments:
#	id	Identifier of play list, as returned by ::playlist::new
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::playlist::__cleanup { id } {
    variable PLS
    variable log

    # Check if this is one of our playlists.
    set idx [lsearch $PLS(playlists) $id]
    if { $idx < 0 } {
	${log}::warn "$id is not a valid playlist"
	return ""
    }

    # Get to its information
    set varname "::playlist::pls_$id"
    upvar \#0 $varname PlayList

    ${log}::debug "Cleaning up old sounds from $id: $PlayList(destroy_onnext)"
    foreach snd $PlayList(destroy_onnext) {
	$snd destroy
    }

    set PlayList(destroy_onnext) ""
}


# ::playlist::__pick -- Pick a file from file list
#
#	Pick the "next" file in one direction or another in the play
#	list.  Directions of more than "1" are allowed here!
#
# Arguments:
#	id	Identifier of play list, as returned by ::playlist::new
#	dir	Direction (will be passed to incr!)
#	fid	File identifier to pick from (empty means current)
#
# Results:
#	Return the identifier of the "next" file.
#
# Side Effects:
#	None.
proc ::playlist::__pick { id { dir 0 } { fid "" } } {
    variable PLS
    variable log

    # Check if this is one of our playlists.
    set idx [lsearch $PLS(playlists) $id]
    if { $idx < 0 } {
	${log}::warn "$id is not a valid playlist"
	return ""
    }

    # Get to its information
    set varname "::playlist::pls_$id"
    upvar \#0 $varname PlayList

    # Fixes origin of pick operation so that an empty identifier is
    # understood as the current one.
    if { $fid == "" } {
	set fid $PlayList(current)
    }

    # Increment in the right direction and return the next identifier
    # or return an empty one.
    set idx [lsearch $PlayList(files) $fid]
    if { $idx < 0 } {
	set next ""
    } else {
	incr idx $dir
	set next [lindex $PlayList(files) $idx]
	if { $next == "" } {
	    if { [string is true $PlayList(repeat)] } {
		if { $dir >= 0 } {
		    set next 0
		} else {
		    set next end
		}
	    } else {
		set next ""
	    }
	}
    }

    ${log}::debug "Picking after $fid in playlist $id (dir: $dir) = $next"

    return $next
}


# ::playlist::next -- Jump to next file and play
#
#	Jump to the next file in the playlist and start playing from
#	there.  Any current playing operation will be immediately
#	aborted.
#
# Arguments:
#	id	Identifier of play list, as returned by ::playlist::new
#
# Results:
#	Identifier of next playing file, or empty
#
# Side Effects:
#	None.
proc ::playlist::next { id } {
    set next [::playlist::__pick $id 1]
    if { $next != "" } {
	return [::playlist::play $id $next]
    } else {
	::playlist::stop $id
	return ""
    }
}


# ::playlist::previous -- Jump to previous file and play
#
#	Jump to the previous file in the playlist and start playing
#	from there.  Any current playing operation will be immediately
#	aborted.
#
# Arguments:
#	id	Identifier of play list, as returned by ::playlist::new
#
# Results:
#	Identifier of previous playing file, or empty
#
# Side Effects:
#	None.
proc ::playlist::previous { id } {
    set next [::playlist::__pick $id -1]
    if { $next != "" } {
	return [::playlist::play $id $next]
    } else {
	::playlist::stop $id
	return ""
    }
}


# ::playlist::__picknext -- Pick next file and play on play end.
#
#	This command is called upon the end of a snack play operation.
#	It picks the next file in the playlist and plays it.
#
# Arguments:
#	id	Identifier of play list, as returned by ::playlist::new
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::playlist::__picknext { id } {
    variable PLS
    variable log

    # Check whether the playlist is still there...
    set idx [lsearch $PLS(playlists) $id]
    if { $idx < 0 } {
	${log}::warn "$id is not a valid playlist"
	return ""
    }

    # Get the identifier of the next one.
    set next [::playlist::__pick $id 1]

    # Force playing to be empty, we know that for sure since we are
    # called as a callback from snack at the end of the sound play.  If
    # we don't do that and call stop on a stopped file sound in snack,
    # snack seems to crash.  We would have been able to simply call
    # play with the next one otherwise.
    set varname "::playlist::pls_$id"
    upvar \#0 $varname PlayList
    set PlayList(playing) ""
	
    if { $next != "" } {
	# Arrange to play next file
	::playlist::play $id $next
    } else {
	::playlist::stop $id
    }
}


# ::playlist::__getsound -- Get snack sound for play list file
#
#	This command returns the snack sound associated to a file from
#	the play list.
#
# Arguments:
#	id	Identifier of play list, as returned by ::playlist::new
#	fid	Identifier of file to get sound for, empty means currently
#               playing one.
#
# Results:
#	Returns an empty string on error, or the identifier of the
#	snack sound
#
# Side Effects:
#	None.
proc ::playlist::__getsound { id { fid "" } } {
    variable PLS
    variable log

    # Is this one of our playlists?
    set idx [lsearch $PLS(playlists) $id]
    if { $idx < 0 } {
	${log}::warn "$id is not a valid playlist"
	return ""
    }

    # Get to information on the list.
    set varname "::playlist::pls_$id"
    upvar \#0 $varname PlayList
    
    # Fix identifier to get sound for.
    if { $fid == "" } {
	set fid $PlayList(current)
    }

    # If we have an identifier, it identifies one of our recognised
    # files (i.e. check for file membership in playlist) and if we
    # have a snack sound for it, return the sound object, otherwise,
    # return an empty string.
    if { $fid != "" } {
	set idx [lsearch $PlayList(files) $fid]
	if { $idx >= 0 } {
	    set varname "::playlist::files_${id}_${fid}"
	    upvar \#0 $varname File
	    
	    if { $File(sound) != "" } {
		${log}::debug "Sound for $fid in playlist $id is $File(sound)"
		return $File(sound)
	    }
	} else {
	    ${log}::warn "Current $fid does not exist anymore in playlist $id"
	}
    }

    return ""
}


# ::playlist::elapsed -- How many milliseconds listened from?
#
#	This command computes an approximate value for a playing
#	pointer within a given file and returns this value (in
#	milliseconds since the beginning of the file).  The command is
#	aware of the pauses.
#
# Arguments:
#	id	Identifier of play list, as returned by ::playlist::new
#	fid	File identifier (empty means current).
#
# Results:
#	The playing pointer within the file, a negative number is the
#	file is not being played.
#
# Side Effects:
#	None.
proc ::playlist::elapsed { id { fid "" } } {
    variable PLS
    variable log

    set idx [lsearch $PLS(playlists) $id]
    if { $idx < 0 } {
	${log}::warn "$id is not a valid playlist"
	return -1
    } else {
	# Get to information on the list.
	set varname "::playlist::pls_$id"
	upvar \#0 $varname PlayList

	if { $fid == "" } {
	    set fid $PlayList(current)
	}

	if { $PlayList(current) == "" } {
	    return -1
	}

	if { $PlayList(current) == $fid } {
	    set now [clock clicks -milliseconds]
	    if { $PlayList(play_mode) == "PLAYING" } {
		set since [expr $now - $PlayList(play_start)]
		return [expr $PlayList(elapsed) + $since]
	    } elseif { $PlayList(play_mode) == "PAUSED" } {
		return $PlayList(elapsed)
	    } else {
		# STOPPED
		return -1
	    }
	} else {
	    return -1
	}
    }

    return -1;  # Never reached
}


# ::playlist::stop -- Stop current playing
#
#	This command stops the current playlist and inherently resets
#	the current playing pointer.  If any file is currently being
#	played, the playing operation will be aborted.
#
# Arguments:
#	id	Identifier of play list, as returned by ::playlist::new
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::playlist::stop { id } {
    variable PLS
    variable log

    set idx [lsearch $PLS(playlists) $id]
    if { $idx < 0 } {
	${log}::warn "$id is not a valid playlist"
    } else {
	# Get to information on the list.
	set varname "::playlist::pls_$id"
	upvar \#0 $varname PlayList

	set now [clock clicks -milliseconds]
	if { $PlayList(playing) != "" } {
	    $PlayList(playing) stop
	}

	if { $PlayList(play_mode) == "PLAYING" } {
	    set since [expr $now - $PlayList(play_start)]
	    incr PlayList(elapsed) $since
	}

	if { $PlayList(current) != "" } {
	    __do_cbs $id END $PlayList(current) $PlayList(elapsed)
	}

	set PlayList(playing) ""
	set PlayList(current) ""
	set PlayList(elapsed) -1
	set PlayList(play_start) -1
	set PlayList(play_mode) STOPPED
    }

    ::playlist::__cleanup $id
}


# ::playlist::pause -- Pause current playing
#
#	This command pauses the playing of the current file being
#	played from the playlist, if any.  Any successive call to this
#	command will resume playing from this very point.
#
# Arguments:
#	id	Identifier of play list, as returned by ::playlist::new
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::playlist::pause { id } {
    variable PLS
    variable log

    set idx [lsearch $PLS(playlists) $id]
    if { $idx < 0 } {
	${log}::warn "$id is not a valid playlist"
    } else {
	# Get to information on the list.
	set varname "::playlist::pls_$id"
	upvar \#0 $varname PlayList

	if { $PlayList(playing) != "" } {
	    set now [clock clicks -milliseconds]
	    $PlayList(playing) pause

	    if { $PlayList(play_mode) == "PLAYING" } {
		set operation PAUSE
		set since [expr $now - $PlayList(play_start)]
		incr PlayList(elapsed) $since
		set PlayList(play_mode) PAUSED
	    } elseif { $PlayList(play_mode) == "PAUSED" } {
		set operation RESUME
		set PlayList(play_start) $now
		set PlayList(play_mode) PLAYING
	    }

	    if { $PlayList(current) != "" } {
		__do_cbs $id $operation $PlayList(current) $PlayList(elapsed)
	    }
	}
    }
}


# ::playlist::current -- Get identifier of currently playing file
#
#	Get identifier of currently playing file in play list.
#
# Arguments:
#	id	Identifier of play list, as returned by ::playlist::new
#
# Results:
#	The identifier of the file currently playing, or an empty
#	string if none.
#
# Side Effects:
#	None.
proc ::playlist::current { id } {
    variable PLS
    variable log

    # Is this one of our playlists?
    set idx [lsearch $PLS(playlists) $id]
    if { $idx < 0 } {
	${log}::warn "$id is not a valid playlist"
	return ""
    }

    # Get to information on the list.
    set varname "::playlist::pls_$id"
    upvar \#0 $varname PlayList
    
    return $PlayList(current)
}


# ::playlist::setbounds -- Set start and stop time for sound
#
#	Set the start and end playing time for a sound file within the
#	playlist.
#
# Arguments:
#	id	Identifier of play list, as returned by ::playlist::new
#	fid	Identifier of file in play list.
#	start	Start time (in milliseconds since file beginning)
#	end	End time (in milliseconds since file beginning)
#
# Results:
#	None
#
# Side Effects:
#	When stepping through the playing only samples within the
#	bounds will be played!
proc ::playlist::setbounds { id fid { start 0 } { end end } } {
    variable PLS
    variable log

    set idx [lsearch $PLS(playlists) $id]
    if { $idx < 0 } {
	${log}::warn "$id is not a valid playlist"
	return ""
    }
    
    set varname "::playlist::pls_$id"
    upvar \#0 $varname PlayList

    foreach i_fid $PlayList(files) {
	if { $i_fid == $fid } {
	    set varname "::playlist::files_${id}_${fid}"
	    upvar \#0 $varname File

	    set File(snd_start) $start
	    set File(snd_end) $end
	    if { ![string is double $File(snd_start)] \
		     || $File(snd_start) < 0 } {
		set File(snd_start) 0
	    }
	    if { ![string is double $File(snd_end)] || $File(snd_end) < 0 } {
		set File(snd_end) end
	    }
	}
    }
}


# ::playlist::play -- Play current play list
#
#	Start playing the current play list from current location
#
# Arguments:
#	id	Identifier of play list, as returned by ::playlist::new
#	tag	Tag of sound at which we should start playing, defaults to 1st
#
# Results:
#	Returns the identifier of the file being played, or an empty
#	string if none was found or if the playing operation failed.
#
# Side Effects:
#	Will use snack to start playing files in order
proc ::playlist::play { id { tag "" } } {
    variable PLS
    variable log

    ::playlist::stop $id

    # Choose identifier
    set fid [::playlist::__pick $id 0 [lindex [::playlist::get $id $tag] 0]]
    
    set idx [lsearch $PLS(playlists) $id]
    if { $idx < 0 } {
	${log}::warn "$id is not a valid playlist"
	return ""
    }

    set varname "::playlist::pls_$id"
    upvar \#0 $varname PlayList

    if { $fid == "" } {
	set fid [lindex $PlayList(files) 0]
	if { $fid == "" } {
	    return ""
	}
    }
    ${log}::debug "Starting playing $fid in playlist $id"

    set varname "::playlist::files_${id}_${fid}"
    upvar \#0 $varname File
    if { $File(sound) == "" } {
	if { [catch {::snack::sound -rate $PlayList(rate) \
			 -file $File(fname)} snd] } {
	    ${log}::warn "Could not create sound object for $File(fname): $snd"
	} else {
	    set File(sound) $snd
	}
    }

    # Managed to create a sound, start playing it.
    if { $File(sound) != "" } {
	# Remember the identifier of the file and the sound that we
	# are playing.
	set PlayList(current) $fid
	set PlayList(playing) $File(sound)

	# Compute where we should start and stop within the sound.
	# Snack expresses things in samples, the interface against the
	# user is in milliseconds.
	set snd_len_msecs [expr 1000 * [$File(sound) length -u seconds]]
	set snd_len_samples [$File(sound) length -u samples]
	set f [expr double($snd_len_samples) / $snd_len_msecs]
	set start [expr int($File(snd_start) * $f)]
	if { $File(snd_end) == "end" || $File(snd_end) == "" } {
	    set end -1
	} else {
	    set end [expr int($File(snd_end) * $f)]
	}

	# Do the play callback we do it now so that even if the
	# callbacked commands take time, their time will not be taken
	# into consideration when guessing elapsed time.
	__do_cbs $id PLAY $fid $File(snd_start)

	# Prepare elapsed time calculation state
	set PlayList(elapsed) $File(snd_start)
	set PlayList(play_start) [clock clicks -milliseconds]
	set PlayList(play_mode) PLAYING; # could be PAUSED later
	
	# Now start playing the sound ASAP and see to register a
	# command to know when we will be done.
	$File(sound) play -blocking 0 -start $start -end $end \
	    -command "::playlist::__picknext $id" 
    }

    return $PlayList(current)
}


# ::playlist::clear -- Clear a play list
#
#	Clear a play list.
#
# Arguments:
#	id	Identifier of play list, as returned by ::playlist::new
#	stop	Should we stop the current file being played (if appropriate)?
#
# Results:
#	None.
#
# Side Effects:
#	Can call stop.
proc ::playlist::clear { id { stop off } } {
    if { [string is true $stop] } {
	::playlist::stop $id
    }

    ::playlist::remove $id * on
}

		       
# ::playlist::new -- Create a new play list.
#
#	Create a new play list and return an identifier that will be
#	used in each further call to this library.
#
# Arguments:
#	None.
#
# Results:
#	Return a unique identifier for the playlist.
#
# Side Effects:
#	None.
proc ::playlist::new { { repeat off } { rate 16000 } } {
    variable PLS
    variable log

    set id [incr PLS(idgene)]
    set varname "::playlist::pls_$id"
    upvar \#0 $varname PlayList
    
    set PlayList(id) $id
    set PlayList(idgene) 0
    set PlayList(files) ""
    set PlayList(current) ""
    set PlayList(playing) ""
    set PlayList(rate) $rate
    set PlayList(repeat) $repeat
    set PlayList(destroy_onnext) ""
    set PlayList(callbacks) ""
    set PlayList(elapsed) -1
    set PlayList(play_start) -1
    set PlayList(play_mode) STOPPED

    lappend PLS(playlists) $id

    return $id
}


# ::playlist::flow_watch -- Watch file flow within a playlist
#
#	This command registers callbacks that can be used to monitor
#	the file flow within a play list.  The callbacks will be
#	called at key operation points such as addition or removal of
#	files or start and end of playing.  The operations supported
#	are the following: ADD, REMOVE, PLAY, END, PAUSE and RESUME.
#	All callbacks matching a given operation names will be called
#	with the identifier of the playlist, the name of the operation
#	and the identifier of the file in question.  The playing
#	related operations additionally take an approximate
#	milliseconds counter telling where in the file the operation
#	took place.
#
# Arguments:
#	id	Identifier of play list, as returned by ::playlist::new
#	op_ptn	string match like pattern for operation name matching.
#	cmd	Command to call, arguments will be as described above.
#
# Results:
#	Returns 0 or 1 to mediate failure or success.
#
# Side Effects:
#	None.
proc ::playlist::flow_watch { id op_ptn cmd } {
    variable PLS
    variable log

    set idx [lsearch $PLS(playlists) $id]
    if { $idx < 0 } {
	${log}::warn "$id is not a valid playlist"
	return 0
    }
    
    set varname "::playlist::pls_$id"
    upvar \#0 $varname PlayList

    lappend PlayList(callbacks) "$op_ptn" "$cmd"

    return 1
}
