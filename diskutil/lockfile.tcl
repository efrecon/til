# lockfile.tcl --
#
#	A simplistic lockfile implementation
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.


package require Tcl 8.2
package require logger
package require diskutil

package provide lockfile 1.0

namespace eval ::lockfile {
    
    # Initialise global state.
    variable LockFile
    if {![info exists LockFile]} {
	array set LockFile {
	    loglevel           warn
	    idgene             0
	    -sleeptime         8
	    -retries           -1
	    -locktimeout       1024
	    -suspend           16
	    -callbacks         ""
	}
	variable log [::logger::init lockfile]
	${log}::setlevel $LockFile(loglevel)
    }

    # Export commands
    namespace export loglevel acquire release defaults
}


# ::lockfile::loglevel -- Set/Get current log level.
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
proc ::lockfile::loglevel { { loglvl "" } } {
    variable LockFile
    variable log

    if { $loglvl != "" } {
	if { [catch "${log}::setlevel $loglvl"] == 0 } {
	    set LockFile(loglevel) $loglvl
	}
    }

    return $LockFile(loglevel)
}


# ::lockfile::config -- Configure a lock acquisition
#
#	This command set or get the options of a lock acquisition
#
# Arguments:
#	id	acquisition identifier
#	args	list of options
#
# Results:
#	Return all options, the option requested or set the options
#	(and return the list of the old values).
#
# Side Effects:
#	None.
proc ::lockfile::config { id args } {
    variable LockFile
    variable log

    # Check that this is one of our connections
    set idx [lsearch $LockFile(locks) $id]
    if { $idx < 0 } {
	${log}::warn "Lock acquisition $id is not valid"
	return -code error "Identifier invalid"
    }

    set varname "::lockfile::Lock_${id}"
    upvar \#0 $varname Lock

    set o [lsort [array names Lock "-*"]]

    if { [llength $args] == 0 } {      ;# Return all results
	set result ""
	foreach name $o {
	    lappend result $name $Lock($name)
	}
	return $result
    }

    set oldvalues ""
    foreach {opt value} $args {        ;# Get onr or set some
	if { [lsearch $o $opt] == -1 } {
	    return -code error "Unknown option $opt, must be: [join $o ", " ]"
	}
	if { [llength $args] == 1 } {  ;# Get one config value
	    return $Lock($opt)
	}
	lappend oldvalues $opt $Lock($opt)
	set Lock($opt) $value         ;# Set the config value
    }

    return $oldvalues
}


# ::lockfile::__invoke_cbs -- Invoke all callbacks associated to a lock
#
#	This command invoke all the callbacks associated to a given
#	lock, if any.
#
# Arguments:
#	id	Identifier of existing lock
#	status	Status result (LOCKED or ERROR)
#	msg	Full path to file or error message.
#
# Results:
#	Return number of successfull callbacks made
#
# Side Effects:
#	None.
proc ::lockfile::__invoke_cbs { id status msg } {
    variable LockFile
    variable log

    # Check that this is one of our connections
    set idx [lsearch $LockFile(locks) $id]
    if { $idx < 0 } {
	${log}::warn "Lock acquisition $id is not valid"
	return -code error "Identifier invalid"
    }

    set varname "::lockfile::Lock_${id}"
    upvar \#0 $varname Lock

    # Deliver all callbacks.
    set nb_called 0
    foreach cb $Lock(-callbacks) {
	if { [catch {eval $cb $Lock(id) $status {$msg}} err] } {
		    ${log}::warn \
			"Error when invoking callback on $Lock(fname): $err"
	} else {
	    incr nb_called
	}
    }
    
    return $nb_called
}


# ::lockfile::__check_lock -- Handle locks
#
#	This command forms the core routine of this module.  If the
#	lock can be acquired, it will create the lock file.
#	Otherwise, if the file already exists, it will periodically
#	check for the presence of the file.  In that case it does this
#	with respect to the maximum number of retries and the lock
#	timeout.
#
# Arguments:
#	id	Identifier of existing lock context
#
# Results:
#	Return an error when the lock does not exist.
#
# Side Effects:
#	Create/remove lock file when necessary.
proc ::lockfile::__check_lock { id } {
    variable LockFile
    variable log

    # Check that this is one of our connections
    set idx [lsearch $LockFile(locks) $id]
    if { $idx < 0 } {
	${log}::warn "Lock acquisition $id is not valid"
	return -code error "Identifier invalid"
    }

    set varname "::lockfile::Lock_${id}"
    upvar \#0 $varname Lock
    set Lock(timer) ""

    ${log}::debug "Checking for lockfile \"$Lock(fname)\""

    if { [catch {file mtime $Lock(fname)} mtime] == 0 } {
	# We could get the latest modification time of the file, which
	# means at the same time that it exists.  If we have reached 0
	# in our number of retries, give up on the lock
	# attempts. Otherwise test again.
	if { $Lock(-retries) == 0 } {
	    # Max number of retries reached: give up and arrange for
	    # an error to occur.
	    set Lock(errmsg) \
		"Too many retries when trying to acquire lock \"$Lock(fname)\""
	    __invoke_cbs $id ERROR $Lock(errmsg)
	    set Lock(finished) 1
	} else {
	    # Allowed for yet another retry.  If the timeout is
	    # reached, attempt to remove the lock and let other
	    # process a chance to acquire the lock before us.
	    incr Lock(-retries) -1
	    if { $Lock(-locktimeout) >= 0 } {
		if { [expr {[clock seconds] - $mtime}] \
			 >= $Lock(-locktimeout) } {
		    ${log}::info \
			"Forcing removal of inactive \"$Lock(fname)\""
		    if { [catch {file delete -force -- $Lock(fname)} \
			      err] == 0 } {
			${log}::info \
			    "Checking \"$Lock(fname)\" in\
                             $Lock(-suspend) seconds."
			set Lock(timer) \
			    [after [expr {$Lock(-suspend) * 1000}] \
				 ::lockfile::__check_lock $id]
		    } else {
			set Lock(errmsg) \
			    "Could not force removal of inactive\
                             \"$Lock(fname)\""
			__invoke_cbs $id ERROR $Lock(errmsg)
			set Lock(finished) 1
		    }
		} else {
		    set Lock(timer) \
			[after [expr {$Lock(-sleeptime) * 1000}] \
			     ::lockfile::__check_lock $id]
		}
	    } else {
		set Lock(timer) \
		    [after [expr {$Lock(-sleeptime) * 1000}] \
			 ::lockfile::__check_lock $id]
	    }
	}
    } else {
	# We could not get the latest modification time of the file,
	# which means that it probably does not exist.  Create the
	# lock file if possible, otherwise arrange for an error.
	${log}::debug "No modification time for \"$Lock(fname)\", creating"
	if { [catch {open $Lock(fname) "w"} fd] == 0 } {
	    puts $fd [pid]
	    close $fd

	    __invoke_cbs $id LOCKED $Lock(fname)
	    set Lock(finished) 1
	} else {
	    set Lock(errmsg) "Could not open \"$Lock(fname)\" for writing"
	    __invoke_cbs $id ERROR $Lock(errmsg)
	    set Lock(finished) 1
	}
    }
}


# ::lockfile::name -- Generate a lock file name
#
#	This command generates a name that is appropriate for a lock
#	file.  It sees to have the correct extension and returns the
#	path to a temporary unique location when called with no
#	arguments.
#
# Arguments:
#	lockf	Path to lock file name, empty for unique temporary file name
#
# Results:
#	Return an appropriate file name
#
# Side Effects:
#	None.
proc ::lockfile::name { { lockf "" } } {
    variable log
    global tcl_platform
    
    array set lock_exts [list unix lock windows lck]
    if { $lockf eq "" } {
	set lockf [::diskutil::temporary_file "lockfile" \
		       $lock_exts($tcl_platform(platform))]
    } else {
	set ext [file extension $lockf]
	if { $ext ne $lock_exts($tcl_platform(platform)) } {
	    ${log}::info \
		"Appending $lock_exts($tcl_platform(platform))\
                 as file extension"
	    append lockf ".$lock_exts($tcl_platform(platform))"
	}
    }

    return $lockf
}


# ::lockfile::acquire -- Acquire resource lock via file
#
#	This command locks a resource via a lock file.  It is modelled
#	after the lockfile UNIX command and accepts a set of similar
#	arguments (key options starting with a dash (see below)).  The
#	command can be called in a synchronous or asynchronous manner.
#	If a number of commands are given along the -callbacks option,
#	the lock will be awaited in an asynchronous manner and the
#	callbacks called once the lock has be acquired.  Otherwise,
#	this command will wait until the lock can be acquired (or an
#	error as occured).  If the lock file exists, the command will
#	wait for -sleeptime seconds (defaults to 8) and retry -retries
#	times until it succeeds.  A negative -retries will be
#	understood as an infinite number of times.  If a -locktimeout
#	is specified, the lockfile will be forced for removal
#	-locktimeout seconds after it has last been modified.  After
#	it has been removed, the implementation will wait for -suspend
#	more seconds before acquiring the lock.
#
# Arguments:
#	lockf	Path to lock file name
#	args	List of options as explained above
#
# Results:
#	Return the identifier of the lock operation or an error on
#	failure.
#
# Side Effects:
#	Will possibly create the lock file on disk.
proc ::lockfile::acquire { lockf args } {
    variable LockFile
    variable log
    global tcl_platform

    set id [incr LockFile(idgene)]
    set varname "::lockfile::Lock_${id}"
    upvar \#0 $varname Lock

    set Lock(id) $id
    set Lock(fname) [::diskutil::absolute_path $lockf]
    set Lock(errmsg) ""
    set Lock(rmlock) 0
    foreach opt [array names LockFile "-*"] {
	set Lock($opt) "$LockFile($opt)"
    }
    lappend LockFile(locks) $id
    eval config $id $args

    if { $Lock(-callbacks) eq "" } {
	# No callbacks means synchronous mode.  Wait until the
	# finished index of the array is modified and arrange for
	# regularily checking the lock file.
	set Lock(finished) 0
	set Lock(timer) [after idle ::lockfile::__check_lock $id]
	vwait ::lockfile::Lock_${id}(finished)
	if { $Lock(errmsg) eq "" } {
	    # No error, we locked the file, return the identifier of
	    # the lock
	    return $id
	} else {
	    # We have an error message, remove the lock context and
	    # return an error.
	    set idx [lsearch $LockFile(locks) $id]
	    if { $idx >= 0 } {
		set LockFile(locks) [lreplace $LockFile(locks) $idx $idx]
		unset Lock
	    }
	    return -code error \
		"Could not acquire lock on $Lock(fname): $Lock(errmsg)"
	}
    } else {
	# There are callbacks specified, switch to asynchronous mode.
	__check_lock $id
	return $id
    }

    # Never reached
}



# ::lockfile::release -- Release a lock
#
#	This command release a lock that has been acquired with the
#	::lockfile::acquire command.  The file will be removed from
#	the disk.
#
# Arguments:
#	id	Identifier of lock operation, as returned by ::acquire
#
# Results:
#	Returns an error if the file could not be removed.
#
# Side Effects:
#	Will remove the lock file from disk.
proc ::lockfile::release { id } {
    variable LockFile
    variable log

    # Check that this is one of our connections
    set idx [lsearch $LockFile(locks) $id]
    if { $idx < 0 } {
	${log}::warn "Lock acquisition $id is not valid"
	return -code error "Identifier invalid"
    }

    set varname "::lockfile::Lock_${id}"
    upvar \#0 $varname Lock

    if { $Lock(timer) ne "" } {
	after cancel $Lock(timer)
    }
    
    set rmfile $Lock(fname)
    set LockFile(locks) [lreplace $LockFile(locks) $idx $idx]
    unset Lock

    file delete -force -- $rmfile
}


# ::lockfile::defaults -- Set/Get defaults for all new connections
#
#	This command sets or gets the defaults options for all new
#	coming locks, it will not perpetrate on existing pending
#	locks.  These defaults can be overriden when acquiring a lock
#
# Arguments:
#	args	List of -key value or just -key to get value
#
# Results:
#	Return all options, the option requested or set the options
#
# Side Effects:
#	None.
proc ::lockfile::defaults { args } {
    variable LockFile
    variable log

    set o [lsort [array names LockFile "-*"]]

    if { [llength $args] == 0 } {      ;# Return all results
	set result ""
	foreach name $o {
	    lappend result $name $LockFile($name)
	}
	return $result
    }

    foreach {opt value} $args {        ;# Get onr or set some
	if { [lsearch $o $opt] == -1 } {
	    return -code error "Unknown option $opt, must be: [join $o ,]"
	}
	if { [llength $args] == 1 } {  ;# Get one config value
	    return $LockFile($opt)
	}
	set LockFile($opt) $value           ;# Set the config value
    }
}

