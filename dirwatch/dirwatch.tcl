# dirwatch.tcl --
#
#	Procedures to watch the content of directories for file
#	addition and removal.
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

package require Tcl 8.2
package require logger

package provide dirwatch 1.0

namespace eval ::dirwatch {
    # Variables of name dirwatch::__DirWatch_<id> are created as arrays to
    # support each directory watch.

    # Initialise global state
    variable DirWatch
    if {![info exists DirWatch]} {
	array set DirWatch {
	    forcecheck         30
	    id_generator       0
	    watchs             ""
	    loglevel           warn
	}
	variable log [::logger::init dirwatch]
	${log}::setlevel $DirWatch(loglevel)
    }

    namespace export new delete pause resume loglevel
}





# ::dirwatch::loglevel -- Set/Get current log level.
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
proc ::dirwatch::loglevel { { loglvl "" } } {
    variable Dirwatch
    variable log

    if { $loglvl != "" } {
	if { [catch "${log}::setlevel $loglvl"] == 0 } {
	    set DirWatch(loglevel) $loglvl
	}
    }

    return $DirWatch(loglevel)
}


# ::dirwatch::find --
#
#	Find a directory watch by its content (i.e. the search has to
#	match the exact same directory and exact same file pattern).
#
#
# Arguments:
#	dir     	Path to directory
#	file_ptn	File pattern to match within directory
#
# Results:
#	Return the identifier (variable name, really) for the matching
#	watch or an empty string.
#
# Side Effects:
#	None.
proc ::dirwatch::find { dir { file_ptn * } } {
    variable DirWatch

    foreach w $DirWatch(watchs) {
	upvar \#0 $w Watch
	if { $Watch(directory) == $dir && $Watch(pattern) == $file_ptn } {
	    return $w
	}
    }
    
    return ""
}



# ::dirwatch::check --
#
#	This command performs the core work of this module.  It checks
#	the current content of the directory associated to the watch
#	which identifier is passed as an argument against previous
#	content and calls file addition or removal callbacks.  Checks
#	handles directory suppression.
#
# Arguments:
#	w	Identifier of the watch, as returned by ::dirwatch::new
#
# Results:
#	None.
#
# Side Effects:
#	This command reschedules itself to perform the next periodic
#	check.
proc ::dirwatch::check { w } {
    variable DirWatch
    variable log

    upvar \#0 $w Watch

    # If the watch is currently paused, do not check for anything at
    # all.
    if { $Watch(active) } {
	${log}::debug "Checking $Watch(directory) for content"

	# If the directory has disappeared, deliver delete callbacks for
	# all its known content and empty that list.
	if { ! [file exists $Watch(directory)] } {
	    ${log}::debug "$Watch(directory) has been removed"
	    foreach fname $Watch(content) {
		foreach delcb $Watch(del_cbs) {
		    if { $delcb != "" } {
			uplevel \#0 $delcb $w $fname
		    }
		}
	    }
	    set Watch(content) ""
	} else {
	    # Otherwise, start by checking the directory modification
	    # time against the one from the previous check.  Content
	    # can only differ if the modification time has changed.
	    # This is an optimisation to avoid disk load.
	    set mtime [file mtime $Watch(directory)]
	    if { $Watch(mtime) != $mtime \
		     || ($DirWatch(forcecheck)>0 \
			     && ($Watch(checks) % $DirWatch(forcecheck))==0) } {
		# Directory being watched has changed or we have
		# reached the number of checks for a "manual" check of
		# the directory.  Check its current content against
		# the prior content.  Maybe nothing have changed
		# (other files not matching the pattern or we had
		# forced!).  Deliver callbacks.
		set content \
		    [glob -directory $Watch(directory) -nocomplain -- \
			 $Watch(pattern)]
		foreach fname $content {
		    set idx [lsearch -exact $Watch(content) $fname]
		    if { $idx < 0 } {
			${log}::debug \
			    "$Watch(directory) contains new matching file: $fname"
			foreach addcb $Watch(new_cbs) {
			    if { $addcb != "" } {
				eval $addcb $w {$fname}
			    }
			}
		    }
		}
		foreach fname $Watch(content) {
		    set idx [lsearch -exact $content $fname]
		    if { $idx < 0 } {
			${log}::debug \
			    "Old matching file $fname removed from $Watch(directory)"
			foreach delcb $Watch(del_cbs) {
			    if { $delcb != "" } {
				eval $delcb $w {$fname}
			    }
			}
		    }
		}

		# Remember this content and modification time for next
		# check
		set Watch(content) $content
		set Watch(mtime) $mtime
	    }
	    incr Watch(checks) 
	}
    }

    # Schedule another check later on.
    set Watch(after_id) [after $Watch(period) ::dirwatch::check $w]
}


# ::dirwatch::pause --
#
#	Pause a directory watch, no callbacks will be delivered during
#	a pause.
#
# Arguments:
#	wtch	Identifier of the watch, as returned by ::dirwatch::new
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::dirwatch::pause { wtch } {
    variable log

    upvar \#0 $wtch Watch
    ${log}::info "Paused directory watch $Watch(directory)"
    set Watch(active) 0
}


# ::dirwatch::resume --
#
#	Resume a directory watch.  The callbacks for file deletion and
#	addition during a pause will be delivered shortly after the
#	end of the pause.
#
# Arguments:
#	wtch	Identifier of the watch, as returned by ::dirwatch::new
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::dirwatch::resume { wtch } {
    variable DirWatch
    variable log

    upvar \#0 $wtch Watch
    ${log}::info "Resumed directory watch $Watch(directory)"
    if { $DirWatch(forcecheck) > 0 } {
	set Watch(checks) 0; # Will force a re-check whatsoever the mtime
    }
    set Watch(active) 1
}


# ::dirwatch::new --
#
#	Create a new directory watch object
#
# Arguments:
#	dir     	Full path to directory.
#	newfile_cb	Command to be called when a new file has been
#                       created in the directory.  The command will be
#                       called with the identifier of the watch and the
#                       name of the file as arguments.
#	delfile_cb	Command to be called when a new file has been
#                       deleted from the directory.  The command will be
#                       called with the identifier of the watch and the
#                       name of the file as arguments.
#	file_ptn	Patter (glob style) of files to be considered
#                       (defaults to *, which matches all files)
#	period  	Period between directory checks (in milliseconds)
#
# Results:
#	Return an identifier for the directory watch object.  This
#	identifier will be used in all further call to the library.
#	The identifier will be an empty string in case of an error.
#
# Side Effects:
#	None.
proc ::dirwatch::new { dir newfile_cb delfile_cb { file_ptn * } \
			   { period 500 } } {
    variable DirWatch
    variable log

    if { ! [file isdirectory $dir] } {
	${log}::error "$dir is not an existing directory, cannot watch!"
	return ""
    }

    if { ! [file readable $dir] } {
	${log}::error "Cannot read from $dir, cannot watch!"
	return ""
    }

    set w [::dirwatch::find $dir $file_ptn]
    if { $w == "" } {
	set id [incr DirWatch(id_generator)]
	set w ::dirwatch::__DirWatch_$id
	upvar \#0 $w Watch
	set Watch(active) 1
	set Watch(directory) $dir
	set Watch(pattern) $file_ptn
	set Watch(new_cbs) [list]
	if { $newfile_cb ne "" } {
	    lappend Watch(new_cbs) $newfile_cb
	}
	set Watch(del_cbs) [list]
	if { $delfile_cb ne "" } {
	    lappend Watch(del_cbs) $delfile_cb
	}
	set Watch(period) $period
	set Watch(checks) 0
	set Watch(mtime) [file mtime $Watch(directory)]
	set Watch(content) \
	    [glob -directory $Watch(directory) -nocomplain -- $Watch(pattern)]
	set Watch(after_id) [after $Watch(period) "::dirwatch::check $w"]
	lappend DirWatch(watchs) $w
	${log}::notice "Created directory watch on $dir"
    } else {
	upvar \#0 $w Watch
	lappend Watch(new_cbs) $newfile_cb
	lappend Watch(del_cbs) $delfile_cb
	${log}::notice "Appended callbacks to directory watch on $dir"
    }

    return $w
}


# ::dirwatch::delete --
#
#	Delete an existing directory watch
#
# Arguments:
#	wtch	Identifier of the watch, as returned by ::dirwatch::new
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::dirwatch::delete { wtch } {
    variable DirWatch
    variable log
    
    set idx [lsearch $DirWatch(watchs) $wtch]
    if { $idx >= 0 } {
	upvar \#0 $wtch Watch
	after cancel $Watch(after_id)
	set DirWatch(watchs) [lreplace $DirWatch(watchs) $idx $idx]
	${log}::notice "Removed directory watch on $Watch(directory)"
	unset Watch
    }
}
