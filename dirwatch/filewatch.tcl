# filewatch.tcl --
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

package provide filewatch 1.0

namespace eval ::filewatch {
    # Variables of name filewatch::__FileWatch_<id> are created as arrays to
    # support each file watch.

    # Initialise global state
    variable FileWatch
    if {![info exists FileWatch]} {
	array set FileWatch {
	    id_generator       0
	    watchs             ""
	    loglevel           warn
	}
	variable log [::logger::init filewatch]
	${log}::setlevel $FileWatch(loglevel)
    }

    namespace export new delete pause resume loglevel find
}





# ::filewatch::loglevel -- Set/Get current log level.
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
proc ::filewatch::loglevel { { loglvl "" } } {
    variable FileWatch
    variable log

    if { $loglvl != "" } {
	if { [catch "${log}::setlevel $loglvl"] == 0 } {
	    set FileWatch(loglevel) $loglvl
	}
    }
    
    return $FileWatch(loglevel)
}


# ::filewatch::__find --
#
#	Find a file watch by its exact path
#
#
# Arguments:
#	fname     	Path to file
#
# Results:
#	Return the identifier (variable name, really) for the matching
#	watch or an empty string.
#
# Side Effects:
#	None.
proc ::filewatch::find { fname } {
    variable FileWatch

    foreach w $FileWatch(watchs) {
	upvar \#0 $w Watch
	if { ! $Watch(deleteme) && $Watch(filename) == $fname } {
	    return $w
	}
    }
    
    return ""
}


# ::filewatch::__check --
#
#	This command performs the core work of this module.  It checks
#	the current file associated to the watch which identifier is
#	passed as an argument against previous content and calls file
#	creation or changement stop modification callbacks.
#
# Arguments:
#	w	Identifier of the watch, as returned by ::filewatch::new
#
# Results:
#	None.
#
# Side Effects:
#	This command reschedules itself to perform the next periodic
#	check.
proc ::filewatch::__check { w } {
    variable log

    upvar \#0 $w Watch

    # If the watch is currently paused, do not check for anything at
    # all.
    if { $Watch(active) } {
	${log}::debug "Checking $Watch(filename) for content"

	# If the file exists and the modification time was 0, then it
	# is new so we call the relevant callbacks.
	if { [file exists $Watch(filename)] } {
	    if { $Watch(mtime) == 0 } {
		${log}::debug \
		    "$Watch(filename) has been created or discovered"
		foreach newcb $Watch(new_cbs) {
		    if { $newcb != "" } {
			if { [catch {eval $newcb $w \$Watch(filename)} \
				  err] } {
			    ${log}::error "New file error callback on\
                                           $Watch(filename): $err"
			}
		    }                                       
		}
		set Watch(mtime) [file mtime $Watch(filename)]
		set Watch(size) [file size $Watch(filename)]
		set Watch(lastequal) [clock clicks -milliseconds]
		set Watch(done_stopchg_cbs) 0
	    } else {
		set mtime [file mtime $Watch(filename)]
		set size [file size $Watch(filename)]
		if { $mtime == $Watch(mtime) && $size == $Watch(size) } {
		    set now [clock clicks -milliseconds]
		    if { ! $Watch(done_stopchg_cbs) \
			     && [expr $now - $Watch(lastequal)] \
			     >= $Watch(stopchg_period) } {
			${log}::debug "$Watch(filename) has not changed\
                                       for [expr $now - $Watch(lastequal)] ms."
			foreach cb $Watch(stopchg_cbs) {
			    if { $cb != "" } {
				if { [catch {eval $cb $w \$Watch(filename)} \
					  err] } {
				    ${log}::error "Stop change error cb on\
                                                   $Watch(filename): $err"
				}
			    }
			}
			set Watch(done_stopchg_cbs) 1
		    }
		} else {
		    set Watch(done_stopchg_cbs) 0
		    set Watch(mtime) $mtime
		    set Watch(size) $size
		    set Watch(lastequal) [clock clicks -milliseconds]
		}
		set Watch(mtime) $mtime
	    }
	}
    }


    # Delete watch if requested during one of the callbacks, otherwise
    # schedule a new check on the file later on.
    if { $Watch(deleteme) } {
	variable FileWatch

	set idx [lsearch $FileWatch(watchs) $w]
	if { $idx >= 0 } {
	    set FileWatch(watchs) [lreplace $FileWatch(watchs) $idx $idx]
	    ${log}::notice "Removed file watch on $Watch(filename)"
	    unset Watch
	}
    } else {
	# Schedule another check later on.
	set Watch(after_id) [after $Watch(period) ::filewatch::__check $w]
    }
}


# ::filewatch::pause --
#
#	Pause a file watch, no callbacks will be delivered during
#	a pause.
#
# Arguments:
#	wtch	Identifier of the watch, as returned by ::filewatch::new
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::filewatch::pause { wtch } {
    variable log

    upvar \#0 $wtch Watch
    ${log}::info "Paused file watch $Watch(filename)"
    set Watch(active) 0
}


# ::filewatch::resume --
#
#	Resume a directory watch.  The callbacks for file deletion and
#	addition during a pause will be delivered shortly after the
#	end of the pause.
#
# Arguments:
#	wtch	Identifier of the watch, as returned by ::filewatch::new
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::filewatch::resume { wtch } {
    variable log

    upvar \#0 $wtch Watch
    ${log}::info "Resumed file watch $Watch(filename)"
    set Watch(done_stopchg_cbs) 0
    set Watch(lastequal) [clock clicks -milliseconds]
    set Watch(active) 1
}


# ::filewatch::new --
#
#	Create a new file watch object
#
# Arguments:
#	fname     	Full path to file.
#	newfile_cb	Command to be called when the file has been
#                       created.  The command will be
#                       called with the identifier of the watch and the
#                       name of the file as arguments.
#	stopchg_cb	Command to be called when a file has stopped
#                       changing.  The command will be
#                       called with the identifier of the watch and the
#                       name of the file as arguments.
#       stopchg_period  Period after which a file is decided not to change
#                       anymore (in milliseconds)
#	period  	Period between file presence/size checks
#                       (in milliseconds)
#       
#
# Results:
#	Return an identifier for the file watch object.  This
#	identifier will be used in all further call to the library.
#	The identifier will be an empty string in case of an error.
#
# Side Effects:
#	None.
proc ::filewatch::new { fname newfile_cb stopchg_cb { stopchg_period 5000 } \
			    { period 500 } } {
    variable FileWatch
    variable log

    set w [::filewatch::find $fname]
    if { $w == "" } {
	set id [incr FileWatch(id_generator)]
	set w ::filewatch::__FileWatch_$id
	upvar \#0 $w Watch
	set Watch(active) 1
	set Watch(filename) $fname
	set Watch(new_cbs) [list $newfile_cb]
	set Watch(stopchg_cbs) [list $stopchg_cb]
	set Watch(stopchg_period) $stopchg_period
	set Watch(period) $period
	set Watch(mtime) 0
	set Watch(deleteme) 0
	set Watch(after_id) [after $Watch(period) "::filewatch::__check $w"]
	lappend FileWatch(watchs) $w
	${log}::notice "Created file watch on $fname"
    } else {
	upvar \#0 $w Watch
	lappend Watch(new_cbs) $newfile_cb
	lappend Watch(stopchg_cbs) $stopchg_cb
	${log}::notice "Appended callbacks to file watch on $fname"
    }

    return $w
}


# ::filewatch::delete --
#
#	Delete an existing file watch
#
# Arguments:
#	wtch	Identifier of the watch, as returned by ::filewatch::new
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::filewatch::delete { wtch } {
    variable log

    upvar \#0 $wtch Watch
    ${log}::debug "Scheduled deletion of file watch on $Watch(filename)"
    set Watch(active) 0
    set Watch(deleteme) 1
}
