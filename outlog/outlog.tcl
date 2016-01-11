# outlog.tcl --
#
#	Procedure to manage output to log files and log rotations.
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

package require Tcl 8.2
package require logger

package provide outlog 1.1

namespace eval ::outlog {
    # Variables of name outlog::__outlog_<id> are created as arrays to
    # support each output log file.

    # Initialise global state
    variable OutLog
    if {![info exists OutLog]} {
	array set OutLog {
	    id_generator  0
	    logs          ""
	    loglevel      warn
	}
	variable log [::logger::init outlog]
	${log}::setlevel $OutLog(loglevel)
    }

    namespace export open puts close loglevel
}




# ::outlog::loglevel -- Set/Get current log level.
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
proc ::outlog::loglevel { { loglvl "" } } {
    variable OutLog
    variable log

    if { $loglvl != "" } {
	if { [catch "${log}::setlevel $loglvl"] == 0 } {
	    set OutLog(loglevel) $loglvl
	}
    }

    return $OutLog(loglevel)
}


# ::outlog::open --
#
#	Create a new output log creation object and return a reference
#	to it.
#
# Arguments:
#	logfile Name of log file to handle (empty string or stdout are
#	        understood)
#	rotate	Number of hours before rotating, -1 to switch off
#	keep	Number of log rotation files to keep
#
# Results:
#	Return an identifier for the log rotation object.  This
#	identifier will be used in all further call to the library.
#
# Side Effects:
#	None.
proc ::outlog::open { logfile { rotate -1 } { keep 4 } { maxsize -1 } { inactivity -1} } {
    variable OutLog
    variable log

    # Look if there is not an already existing log rotator for that
    # file.
    foreach id $OutLog(logs) {
	set varname "::outlog::__outlog_$id"
	upvar \#0 $varname Log

	if { $Log(logfile) == $logfile } {
	    set Log(rotate) $rotate
	    set Log(keep) $keep
	    set Log(inactivity) $inactivity
	    return  $id
	}
    }

    # There is none, initialise an outlog object for that file
    set id [incr OutLog(id_generator)]
    set varname "::outlog::__outlog_$id"
    upvar \#0 $varname Log

    set Log(logfile) $logfile
    set Log(accumulator) ""
    if { $logfile == "" || $logfile == "stdout" || $logfile == "-" } {
	set Log(fd) "stdout"
	set Log(start) [clock seconds]
	${log}::debug "Output log to standard out"
    } else {
	if { [file exists $Log(logfile)] } {
	    file stat $Log(logfile) fdata
	    set Log(start) $fdata(atime)
	    ${log}::debug "Appending log data to $Log(logfile)"
	} else {
	    set Log(start) [clock seconds]
	    ${log}::debug "Will create $Log(logfile)"
	}
	set Log(fd) ""
    }
    set Log(rotate) $rotate
    set Log(keep) $keep
    set Log(maxsize) $maxsize
    set Log(inactivity) $inactivity
    set Log(timer) ""
    if { $Log(inactivity) > 0 } {
	set Log(timer) [after $Log(inactivity) [namespace current]::__close $id]
    }

    lappend OutLog(logs) $id

    return $id
}


proc ::outlog::__close { id } {
    variable OutLog
    variable log

    # Check that this is one of our outlog objects.
    set idx [lsearch $OutLog(logs) $id]
    if { $idx < 0 } {
	${log}::error "$id is not the identifier of an outlog!"
	return
    }
    
    # Get to the global that contains all necessary information
    set varname "::outlog::__outlog_$id"
    upvar \#0 $varname Log

    # Close file descriptor if it is opened
    if { $Log(fd) != "" } {
	${log}::info "Spontaneously closing file descriptor to $Log(logfile),\
	              no activity for $Log(inactivity) ms."
	if { [catch "::close $Log(fd)" err] != 0 } {
	    ${log}::warn "Could not properly close $Log(logfile): $err"
	}
	set Log(fd) ""	
    }
    
    # Remember we've triggered so we won't try to cancel an inexisting timer.
    set Log(timer) ""
}


# ::outlog::puts --
#
#	Log a line to the file associated to an outlog object.
#	Performs log rotation if necessary, applicable and requested.
#	Handle files that might have been lost through NFS restarts...
#
# Arguments:
#	id	Identifier of outlog object, as returned by ::outlog::open
#	line	Line to dump to file
#	norot	Do not rotate right now if none zero
#
# Results:
#	Return the number of lines that were dumped to the file
#
# Side Effects:
#	Write the output line to the file associated to the rotation
#	log, possibly opening a new file if it was time for rotation.
proc ::outlog::puts { id line { norotation 0 } } {
    variable OutLog
    variable log

    # Check that this is one of our outlog objects.
    set idx [lsearch $OutLog(logs) $id]
    if { $idx < 0 } {
	${log}::error "$id is not the identifier of an outlog!"
	return 0
    }
    
    # Get to the global that contains all necessary information
    set varname "::outlog::__outlog_$id"
    upvar \#0 $varname Log

    # Schedule automatic closing of file descriptor in near future, if
    # necessary.
    if { $Log(inactivity) > 0 } {
	if { $Log(timer) != "" } {
	    after cancel $Log(timer)
	}
	set Log(timer) [after $Log(inactivity) [namespace current]::__close $id]
    }

    # Record current time and initialise
    set now [clock seconds]
    set dt [clock format $now]
    set outlines 0

    # If the file descriptor is empty (i.e. at start up or after an
    # NFS failure was discovered), try to reopen the file.
    if { $Log(fd) == "" } {
	if { [catch "::open $Log(logfile) a+" fd] == 0 } {
	    ${log}::notice "$Log(logfile) opened for logging"
	    set Log(fd) $fd
	}
    }

    # If we have an opened file descriptor to output to, do that,
    # otherwise accumulate until we get back to normal.
    if { $Log(fd) != "" } {

	# The accumulator wasn't empty, which means that we have just
	# recovered back to normal.  Dump back the content of the
	# accumulator to the file, together with some recovery
	# message.
	if { $Log(accumulator) != "" } {
	    ${log}::notice "$Log(logfile) reopened, dumping accumulator"
	    ::puts $Log(fd) \
		"RECOVERED at $dt: Reopened $Log(logfile), dumping accumulator"
	    incr outlines
	    foreach l $Log(accumulator) {
		::puts $Log(fd) $l
		incr outlines
	    }
	    set Log(accumulator) ""
	}

	# Output the line to the file.
	::puts $Log(fd) $line
	incr outlines

	# Flush output at once.  We catch this and it may fail.  If it
	# fails, enter output accumulation mode.
	if { [catch "flush $Log(fd)"] != 0 } {
	    catch "::close $Log(fd)"
	    set Log(fd) ""
	    ${log}::warn "Failed writing to $Log(logfile), accumulating"
	    lappend Log(accumulator) \
		"ERROR at $dt: Lost connection to $Log(logfile), accumulating"
	    lappend Log(accumulator) $line
	    set outlines 0
	}
    } else {
	lappend Log(accumulator) $line
    }

    if { ! $norotation } {
	rotate $id $now
    }

    return $outlines
}


proc ::outlog::rotate { id { now "" } } {
    variable OutLog
    variable log

    # Check that this is one of our outlog objects.
    set idx [lsearch $OutLog(logs) $id]
    if { $idx < 0 } {
	${log}::error "$id is not the identifier of an outlog!"
	return 0
    }
    
    # Get to the global that contains all necessary information
    set varname "::outlog::__outlog_$id"
    upvar \#0 $varname Log

    if { $now eq "" } {
	set now [clock seconds]
    }

    # Now takes care of rotations when possible and requested.
    if { $Log(fd) != "" && $Log(fd) != "stdout" } {
	set rotate 0
	# We need to rotate, enough time has elapsed since start.
	if { $Log(rotate) >= 0 } {
	    set elapsed [expr $now - $Log(start)]
	    if { $elapsed >= [expr int($Log(rotate) * 3600)] } {
		${log}::info "Will rotate: $elapsed s. since last rotation."
		set rotate 1
	    }
	}
	if { !$rotate && $Log(maxsize) > 0 } {
	    set sz [file size $Log(logfile)]
	    if { $sz >= $Log(maxsize) } {
		${log}::info "Will rotate: file is $sz bytes"
		set rotate 1
	    }
	}

	if { $rotate } {
	    if { [catch "::close $Log(fd)"] == 0 } {
		${log}::notice "Rotating log files for $Log(logfile)"
		# Set the file descriptor to be empty, it will be
		# reopened next time.
		set Log(fd) ""

		# And performs rotation through renaming the old
		# existing files.  This assumes that we can access
		# them.  On unix, arrange for compressing the oldest
		# files using gzip.
		if { $Log(keep) > 2 } {
		    for { set i [expr $Log(keep) - 1]} { $i > 0 } \
			{ incr i -1 } {
			    if { [file exists "$Log(logfile).$i"] } {
				file rename -force -- \
				    "$Log(logfile).$i" \
				    "$Log(logfile).[expr $i + 1]"
			    }
			    if { [file exists "$Log(logfile).${i}.gz"] } {
				file rename -force -- \
				    "$Log(logfile).${i}.gz" \
				    "$Log(logfile).[expr $i + 1].gz"
			    }
			    if { $i >= 1 \
				     && [file exists \
					     "$Log(logfile).[expr $i+1]"] } {
				if { $::tcl_platform(platform) == "unix" } {
				    set cmd [list gzip -f \
						 "$Log(logfile).[expr $i+1]" &]
				    if { [catch [linsert $cmd 0 exec] err] } {
					${log}::warn "Could not start gzip:\
                                                      $err"
				    }
				}
			    }
			}
		}

		# Finally move or delete the current log file.  It
		# takes position 1 in the ordered list of logs,
		# alternatively, if we did not wish to keep logs, it
		# is removed.
		if { $Log(keep) >= 1 } {
		    file rename -force -- "$Log(logfile)" "$Log(logfile).1"
		} else {
		    file delete -force -- "$Log(logfile)"
		}

		# Do not forget to remember that we have rotated and
		# reinitialise the timer.
		set Log(start) $now
	    } else {
		# We could not close, write an information to the
		# file, there is probably something wrong, so we
		# recurse to benefit from the NFS protection measures.
		${log}::warn "Could not close $Log(logfile) before rotation"
		::outlog::puts $id \
		    "ERROR at $dt: Cannot close $Log(logfile) for rotation" \
		    1
	    }
	}
    }
}

# ::outlog::close --
#
#	Close an outlog object and its associated log file, if necessary.
#
# Arguments:
#	id	Identifier of outlog object, as returned by ::outlog::open
#
# Results:
#	0 if the file could not correctly be closed.
#
# Side Effects:
#	None.
proc ::outlog::close { id } {
    variable OutLog
    variable log

    # Check that this is one of our outlog objects.
    set idx [lsearch $OutLog(logs) $id]
    if { $idx < 0 } {
	${log}::error "$id is not the identifier of an outlog!"
	return 0
    }
    
    # Get to the global that contains all necessary information
    set varname "::outlog::__outlog_$id"
    upvar \#0 $varname Log

    # Close file and clean up
    set res 0
    if { $Log(fd) != "stdout" } {
	# If the file is an empty string close will fail as we wish it
	# will
	set res [catch "::close $Log(fd)"]
    }
    if { $res == 0 } {
	${log}::notice "Log $Log(logfile) successfully closed for output"
    } else {
	${log}::warn "Could not close $Log(logfile) properly"
    }
    if { $Log(timer) != "" } {
	after cancel $Log(timer)
    }
    set OutLog(logs) [lreplace $OutLog(logs) $idx $idx]
    unset Log


    return [expr ! $res]
}

