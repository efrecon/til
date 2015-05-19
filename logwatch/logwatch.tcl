# logwatch.tcl --
#
#	This module contains commands for the dynamic watching of
#	growing log files
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

package require Tcl 8.2
package require logger

package provide logwatch 1.0

namespace eval ::logwatch {
    # Variables of name ::logwatch::__LogWatch_<id> are created as
    # arrays to support each watched log file.

    # Initialise global state
    variable LogWatch
    if {![info exists LogWatch]} {
	array set LogWatch {
	    watchs    ""
	    w_id      0
	    period    250
	    after_id  ""
	    loglevel  warn
	}
	variable log [::logger::init logwatch]
	${log}::setlevel $LogWatch(loglevel)
    }

    namespace export new delete period info
}


# ::logwatch::__readall --
#
#	This command reads data from all registered log files and send
#	the matching read data to all callbacks.
#
# Arguments:
#	None.
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::logwatch::__readall { } {
    variable LogWatch
    variable log
    
    # For each registered log watch, check for data.
    foreach id $LogWatch(watchs) {
	# See to point at information in Watch table variable.
	set varname "::logwatch::__LogWatch_$id"
	upvar \#0 $varname Watch

	# Now open the log file in non blocking mode (line
	# buffering). Take care of the stdin special case.
	set fd ""
	if { $Watch(fname) == "" || $Watch(fname) == "-" \
		 || $Watch(fname) == "stdin" } {
	    set fd stdin
	} else {
	    set res [catch "open $Watch(fname)" fd]
	    if { $res == 0 } {
		fconfigure $fd -blocking no -buffering line
	    }
	}

	# If we managed to open any file, consume everything that is
	# available for reading.
	if { $fd != "" } {
	    if { $fd != "stdin" } {
		# stat() the current file and try being intelligent
		# about file (re)creation.
		file stat $Watch(fname) stat
		if { $Watch(newfile) } {
		    # This file is a file that has just been added to
		    # the log watching service.  Seek to its end and
		    # remember that it is not a new file anymore.
		    ${log}::notice \
			"Starting watching \"$Watch(fname)\" from its end"
		    seek $fd 0 end
		    set Watch(newfile) 0
		} else {
		    # Otherwise, compare the current size and
		    # modification time of the file.  If the current
		    # size is smaller that the previous one or if the
		    # file has been modified since then, then we are
		    # probably facing a log rotation
		    # case. Consequently, we go to the beginning of
		    # the file. Otherwise, seek back to where we were
		    # last time we read something from the file.
		    if { $stat(size) < $Watch(size) } {
			${log}::notice "File \"$Watch(fname)\" has shrunk from $Watch(size) to $stat(size), reading from start"
			seek $fd 0
		    } elseif { $stat(size) == $Watch(size) \
				   && $stat(mtime) != $Watch(mtime) && 0 } {
			${log}::notice "At $stat(mtime), file \"$Watch(fname)\" has been modified since $Watch(mtime) without size change, reading from start"
			seek $fd 0
		    } else {
			seek $fd $Watch(pos) start
		    }
		}
		# Remember current size and time for next iteration.
		set Watch(size) $stat(size)
		set Watch(mtime) $stat(mtime)
	    }
	    
	    # Now consume all available lines and deliver callbacks if
	    # necessary.  Segregate properly between end of file and
	    # empty lines.
	    set done 0
	    while { ! $done } {
		set sz [gets $fd line]
		if { [eof $fd] } {
		    set done 1
		    if { $Watch(eof) != "" } {
			if { [catch {eval [linsert $Watch(eof) end $id]} err]} {
			    ${log}::warn "Error when calling back EOF: $err"
			}
		    }
		} elseif { $sz < 0 } {
		    set done 1
		} else {
		    # We have some data, if the line matches the
		    # regular expression, deliver the callback
		    if { [regexp $Watch(rxp) $line] } {
			uplevel \#0 $Watch(cb) $Watch(id) [list $line]
		    }
		}
	    }

	    # Remember where we are in the file and close it. We will
	    # reopen at the next iteration.  We open and close at each
	    # iteration in order to be able to handle NFS properly.
	    # This is modelled after the GNU implementation of the
	    # tail utility.  See http://wiki.tcl.tk/1154 and
	    # http://wiki.tcl.tk/811 for more information.
	    if { $fd != "stdin" } {
		set Watch(pos) [tell $fd]
		catch "close $fd"
	    }
	}
    }
	

    # Post input reading again.
    set LogWatch(after_id) [after $LogWatch(period) ::logwatch::__readall]
}




# ::logwatch::new --
#
#	Add a watch on a file.  The watch will exist and persist even
#	though the file does not exist.  As soon as the file starts
#	existing, callbacks will be issued for each line that matches
#	the regular expression given as a parameter. The callback will
#	take the identifier of the watch and the line as parameters.
#
# Arguments:
#	file	Path to file to watch.
#	cb	Command to call back when something is read from the file
#	rxp	Regular expression that lines should match for cb to occur
#
# Results:
#	Return an identifier for the log rotation object.  This
#	identifier will be used in all further call to the library.
#
# Side Effects:
#	None.
proc ::logwatch::new { file cb { rxp "" } } {
    variable LogWatch
    variable log

    # Register a new watch
    set id [incr LogWatch(w_id)]
    set varname "::logwatch::__LogWatch_$id"
    upvar \#0 $varname Watch

    # Remember information about the watch
    lappend LogWatch(watchs) $LogWatch(w_id)
    set Watch(id) $LogWatch(w_id)
    set Watch(fname) $file
    set Watch(cb) $cb
    set Watch(eof) ""
    set Watch(rxp) $rxp
    set Watch(newfile) 1
    ${log}::notice "Started to watch $Watch(fname) for dynamic content"

    # Start polling timer if necessary
    if { $LogWatch(after_id) == "" } {
	set LogWatch(after_id) [after idle ::logwatch::__readall]
	${log}::debug "Started regular poll of watched files"
    }

    return $Watch(id)
}



# ::logwatch::delete --
#
#	Remove an existing watch.
#
# Arguments:
#	id	Identifier of the log watch object, as returned by
#               ::logwatch::new
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::logwatch::delete { id } {
    variable LogWatch
    variable log

    # Look to see if this is really one of our ids.
    set idx [lsearch $LogWatch(watchs) $id]
    if { $idx >= 0 } {
	# If it is, unregister it and get all necessary information in
	# the Watch variable.
	set LogWatch(watchs) [lreplace $LogWatch(watchs) $idx $idx]
	set varname "::logwatch::__LogWatch_$id"
	upvar \#0 $varname Watch

	# Forget all information.
	${log}::notice "Stopped watching $Watch(fname) for content"
	unset Watch
    }

    # Unregister the polling reading procedure if necessary
    if { [llength $LogWatch(watchs)] == 0 && $LogWatch(after_id) != "" } {
	after cancel $LogWatch(after_id)
	set LogWatch(after_id) ""
	${log}::debug "Stopped regular polling of watched files"
    }

    return ""
}


# ::logwatch::info --
#
#	If called with an argument, this command will look for any
#	matching existing log watch and return information for it.
#	Otherwise, the command will return information for all
#	existing watches.
#
# Arguments:
#	id	Identifier of the log watch object, as returned by
#               ::logwatch::new.  Alternatively, the name of a file
#                being watched.
#
# Results:
#	A list (of lists )that, for each identifier describe the watch
#	that they correspond to.  Each inner list is suitable for an
#	"array set" command and contains the following entries: id
#	filename and regexp.
#
# Side Effects:
#	None.
proc ::logwatch::info { { id "" } } {
    variable LogWatch

    if { $id == "" } {
	set l ""
	foreach id $LogWatch(watchs) {
	    lappend l [::logwatch::info $id]
	}
	return $l
    } else {
	set found 1
	set idx [lsearch $LogWatch(watchs) $id]
	if { $idx < 0 } {
	    set found 0
	    foreach wid $LogWatch(watchs) {
		set varname "::logwatch::__LogWatch_$wid"
		upvar \#0 $varname Watch
		
		if { [string match $id $Watch(fname)] } {
		    set id $wid
		    set found 1
		    break
		}
	    }
	}
	if { $found } {
	    set varname "::logwatch::__LogWatch_$id"
	    upvar \#0 $varname Watch
	    
	    return [list id $id filename $Watch(fname) regexp "$Watch(rxp)"]
	}
    }

    return ""
}


# ::logwatch::oneof -- EOF callback
#
#       This procedure arranges for a callback to be made everytime
#       EOF has been reached on the file that is being watched.  The
#       identifier of the watch will automatically be appended to the
#       callback.
#
# Arguments:
#	id	Identifier of one of our watches
#	cb	Callback command, empty string to turn off
#
# Results:
#       None.
#
# Side Effects:
#       None.
proc ::logwatch::oneof { id { cb "" } } {
    variable LogWatch
    variable log

    # Look to see if this is really one of our ids.
    set idx [lsearch $LogWatch(watchs) $id]
    if { $idx >= 0 } {
	set varname "::logwatch::__LogWatch_$id"
	upvar \#0 $varname Watch

	set Watch(eof) $cb
    }
}


# ::logwatch::period --
#
#	This command sets or gets the current file watching period (in
#	milliseconds). When setting the period, the command will only
#	have effect whence the current period has elapsed.
#
# Arguments:
#	period	If present and positive, set the current period
#
# Results:
#	Return the current polling period
#
# Side Effects:
#	None.
proc ::logwatch::period { { period -1 } } {
    variable LogWatch
    variable log

    if { $period > 0 } {
	set LogWatch(period) $period
    }
    return $LogWatch(period)
}


# ::logwatch::loglevel -- Set/Get current log level.
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
proc ::logwatch::loglevel { { loglvl "" } } {
    variable LogWatch
    variable log

    if { $loglvl != "" } {
	if { [catch "${log}::setlevel $loglvl"] == 0 } {
	    set LogWatch(loglevel) $loglvl
	}
    }

    return $LogWatch(loglevel)
}


