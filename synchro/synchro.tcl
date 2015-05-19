# synchro.tcl --
#
#	This modules provides a set of routines to synchronise a local
#	directory structure with other remote or locals directory
#	structures so that achieve the same content.
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.


# Implementation and usage notes
# ##############################
#
# A synchronisation is able to handle several sources for files, all
# of which being able to contain a copy of a directory structure.
# These can be added through the add command of this package.  Normal
# users will only add one of these, but several are allowed and
# correctly handled.
#
# A number of callbacks are provided, both to mediate important events
# to the caller and to interact with the caller when some decisions
# are to be taken.
#
# The callbacks for important events are the following:
#
# -newcb: the command attached to this callback will be called with
# two additional arguments whenever a file has been fetched and placed
# in the local synchronised directory structure.  These arguments are
# the identifier of the synchronisation and the name of the file
# (relative or absolute, depending on the -fullpath option).
#
# -startcb: the command attached to this callback will be called with
# four additional arguments whenever a synchronisation process is
# started, i.e. whenever the (remote) source structure is started to
# be traversed for files and directories.  These arguments are the
# identifier of the synchronisation, the type of the source (ftp,
# file, etc.), the URL of the source directory and an iteration
# integer that is increased at each traversal (and thus
# synchronisation) cycle.
#
# -endcb: the command attached to this callback will be called with
# five additional arguments whenever a synchronisation process has
# ended.  These arguments are the identifier of the synchronisation,
# the type of the source (ftp, file, etc.), the URL of the source
# directory, the iteration integer (see above) and the number of
# milliseconds that were spent to perform this synchronisation.
#
# The callbacks for interacting with the caller are the following:
#
# -getcb: the command attached to this callback will be called with
# four additional arguments whenever a synchronisation process wishes
# to get a (remote) file.  These arguments are the identifier of the
# synchronisation, the (relative) name of the file to be considered,
# the local modification time of the current copy of the file (0 when
# the file does not yet exist) and the modification time of the source
# file.  This command should then send back a boolean telling wether
# the synchronisation should attempt to fetch that file from the
# source or not.  If no such command is provided as a callback the
# default is to get the file only if the local (destination) copy is
# older than the (remote) source.
#
# -overwritecb: the command attached to this callback will be called
# with three additional arguments whenever a synchronisation process
# has fetched a (remote) file from the source and there already is a
# copy of that file locally in the destination (synchronised)
# structure.  These arguments are the identifier of the
# synchronisation, the name of the file to be considered, and the name
# of the temporary file that contains the (local) copy of the (remote)
# source file.  This command should then send back a boolean telling
# wether the synchronisation overwrite the already existing file or
# not.  If no such command is provided as a callback the default is to
# overwrite the file.

package require Tcl 8.3
package require logger
package require diskutil
package require md5
package require uri

package provide synchro 1.0

namespace eval ::synchro {
    variable Synchro
    if {![::info exists Synchro]} {
	array set Synchro {
	    loglevel      warn
	    packages      ""
	    idgene        0
	    -consider     ".*"
	    -ignore       ".*~$ .*bak$ .*tmp$"
	    -dndir_pfx    ".dnload"  
	    -dndir_ext    ".tmp"
	    -newcb        ""
	    -startcb      ""
	    -endcb        ""
	    -getcb        ""
	    -overwritecb  ""
	    -fullpath     "on"
	    dirs          ""
	}
	variable log [::logger::init [string trimleft [namespace current] ::]]
	${log}::setlevel $Synchro(loglevel)
    }

    namespace export loglevel config defaults new add delete
}


# ::synchro::loglevel -- Set/Get current log level.
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
proc ::synchro::loglevel { { loglvl "" } } {
    variable Synchro
    variable log

    if { $loglvl != "" } {
	if { [catch "${log}::setlevel $loglvl"] == 0 } {
	    set Synchro(loglevel) $loglvl
	}
	foreach pkg $Synchro(packages) {
	    catch "${pkg}::loglevel $loglvl"
	}
    }

    return $Synchro(loglevel)
}



# ::synchro::__dndir -- Create and return downloading directories
#
#	This command computes, creates and return the downloading
#	directories for a remote synchronisation and (possibly) one of
#	its sources.  Directories for a source are always a sub
#	directory of the synchronisation directory.
#
# Arguments:
#	id	Synchronisation identifier
#	rid	Source identifier within the synchronisation
#
# Results:
#	Directory for the synchronisation or source, an empty string
#	on creation error or an error for worse errors.
#
# Side Effects:
#	Will actively create directories
proc ::synchro::__dndir { id { rid "" } } {
    variable Synchro
    variable log

    # Check that this is one of our synchronisation
    set idx [lsearch $Synchro(dirs) $id]
    if { $idx < 0 } {
	${log}::warn "Synchronisation identifier $id is not valid"
	return -code error "Synchronisation dentifier $id invalid"
    }

    set varname "::synchro::Sync_${id}"
    upvar \#0 $varname Sync

    # Generate name of main download directory
    set dndir $Sync(-dndir_pfx)
    append dndir $Sync(id)
    append dndir $Sync(-dndir_ext)
    set dndir [file join $Sync(dir) $dndir]

    # Create it
    if { [catch {file mkdir $dndir} err] } {
	${log}::warn "Cannot create synchronisation download directory: $err"
	return ""
    }
    
    # Return if no source directory requested.
    if { $rid == "" } {
	return $dndir
    }

    # Check now that this one of our sources
    set idx [lsearch $Sync(sources) $rid]
    if { $idx < 0 } {
	${log}::warn "Synchronisation source identifier $rid is not valid"
	return -code error "Synchronisation source identifier $rid invalid"
    }

    set varname "::synchro::Source_${id}_${rid}"
    upvar \#0 $varname Source

    set srcdir [file join $dndir $rid]

    # Create it
    if { [catch {file mkdir $srcdir} err] } {
	${log}::warn "Cannot create synchronisation source directory: $err"
	return ""
    }
    
    # Return if no source directory requested.
    return $srcdir
}



# ::synchro::__fetched -- Remote URL fetch callback.
#
#	The remote URL has been fetched (or failed to).  Deliver
#	synchronisation callbacks if necessary.
#
# Arguments:
#	id 	Sychronisation identifier
#	rid 	Source identifier
#	dst 	Destination (relative) path
#	howmuch	How many bytes were requested?
#	src 	Full URL path to the source (i.e. remote file)
#	tmpfname 	Temporary name of its current copy
#	status 	Copy status (OK or ERROR)
#	err	Additional error description message
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::synchro::__fetched { id rid dst howmuch src tmpfname status {err {}} } {
    variable Synchro
    variable log

    # Check that this is one of our synchronisation
    set idx [lsearch $Synchro(dirs) $id]
    if { $idx < 0 } {
	${log}::warn "Synchronisation identifier $id is not valid"
	return -code error "Synchronisation dentifier $id invalid"
    }

    set varname "::synchro::Sync_${id}"
    upvar \#0 $varname Sync

    set idx [lsearch $Sync(sources) $rid]
    if { $idx < 0 } {
	${log}::warn "Synchronisation source identifier $rid is not valid"
	return -code error "Synchronisation source identifier $rid invalid"
    }

    set varname "::synchro::Source_${id}_${rid}"
    upvar \#0 $varname Source

    if { $status == "ERROR" } {
	${log}::warn "Could not fetch '$src': $err"
    } else {
	# We managed.  Copy the temporary file into the destination
	# structure.  There is scope to do some additional checks
	# here.  Deliver the callback upon addition/modification (it
	# is the same).
	set dstfname [file join $Sync(dir) $dst]
	if { [file exists $dstfname] || $howmuch >= 0 } {
	    set overwrite [__invokecb $id -overwritecb "$dstfname" "$tmpfname"]
	    if { $overwrite == "" } { set overwrite "on" }
	} else {
	    set overwrite "on"
	}
	if { [string is true $overwrite] } {
	    ${log}::info "Overwriting $dstfname with $tmpfname"
	    if { $howmuch < 0 } {
		if { [file exists $dstfname] } {
		    if { [catch {file delete $dstfname} err] } {
			${log}::warn \
			    "Could not delete old copy $dstfname: $err"
		    }
		}
		if { [catch {file rename $tmpfname $dstfname} err] } {
		    ${log}::warn "Could not move $tmpfname to $dstfname: $err"
		    catch {file delete $tmpfname}
		} else {
		    ${log}::info "Stored $dstfname in synchronised dir"
		    if { [string is true $Sync(-fullpath)] } {
			__invokecb $id -newcb "$dstfname"
		    } else {
			__invokecb $id -newcb "$dst"
		    }
		}
	    } else {
		${log}::info "Decided to overwrite $dst, refetching"
		if { [catch {file delete $tmpfname} err] } {
		    ${log}::warn \
			"Could not delete temporary copy $tmpfname: $err"
		} else {
		    ${log}::debug "Removed temporary copy $tmpfname"
		}
		# Redo the fetch again, whole file this time.
		__fetch $id $rid [file dirname $tmpfname] $dst
	    }
	} else {
	    if { [catch {file delete $tmpfname} err] } {
		${log}::warn "Could not delete temporary copy $tmpfname: $err"
	    } else {
		${log}::debug "Removed temporary copy $tmpfname"
	    }
	}
    }

    # Now make sure the periodical check of that source works.  If we
    # have spent too much time fetching all remote URLs, reschedule
    # immediately.  Otherwise, schedule in a wee while.
    set idx [lsearch $Source(pending) $dst]
    set Source(pending) [lreplace $Source(pending) $idx $idx]
    if { [llength $Source(pending)] == 0 } {
	set since [expr {[clock clicks -milliseconds] - $Source(start)}]
	if { $Source(poll) >= 0 } {
	    set period [expr $Source(poll) * 1000]
	    if { $since > $period } {
		set Source(after) [after idle ::synchro::__dosync $id $rid]
	    } else {
		set Source(after) [after [expr $period - $since] \
				       ::synchro::__dosync $id $rid]
	    }
	}
	__invokecb $id -endcb $Source(type) "$Source(url)" \
	    $Source(nbpolls) $since
	set Source(start) 0
    }
}


# ::synchro::__fetch -- Start fetching a remote url
#
#	Use the package associated to the source to start fetching a
#	remote URL.  Arrange for ::synchro::__fetched to be called
#	upon success or failure.
#
# Arguments:
#	id 	Sychronisation identifier
#	rid 	Source identifier
#	tmpdir	Where to place the temporary copy of the remote URL
#	remotepath	Which (relative) path to fetch.
#       howmuch How many bytes to start fetching
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::synchro::__fetch { id rid tmpdir remotepath { howmuch -1 } } {
    variable Synchro
    variable log

    # Check that this is one of our synchronisation
    set idx [lsearch $Synchro(dirs) $id]
    if { $idx < 0 } {
	${log}::warn "Synchronisation identifier $id is not valid"
	return -code error "Synchronisation dentifier $id invalid"
    }

    set varname "::synchro::Sync_${id}"
    upvar \#0 $varname Sync

    set idx [lsearch $Sync(sources) $rid]
    if { $idx < 0 } {
	${log}::warn "Synchronisation source identifier $rid is not valid"
	return -code error "Synchronisation source identifier $rid invalid"
    }

    set varname "::synchro::Source_${id}_${rid}"
    upvar \#0 $varname Source

    # Construct an appropriate temporary file name for the storage of
    # the current content of the remote URL.
    set ext [file extension $remotepath]
    set tmpfname [file join $tmpdir \
		      [::diskutil::temporary_filename "sync" $ext]]

    # Get the URL.  Arrange for it to call us back upon success or
    # failure.  See to remember that we have a fetch pending.
    $Source(pkg)::geturl $Source(url) $remotepath $tmpfname \
	[list ::synchro::__fetched $id $rid $remotepath $howmuch] "" $howmuch
    lappend Source(pending) $remotepath
}


# ::synchro::__invokecb -- Invoke one of the callbacks
#
#	Invoke one of the callbacks with arguments.
#
# Arguments:
#	id 	Sychronisation identifier
#	cb 	callback to invoke (typically -newcb, -endcb, -startcb)
#	args	Additional arguments.
#
# Results:
#	Return what was returned by the callback, or an empty string on
#       unsuccessfull callback.
#
# Side Effects:
#	None.
proc ::synchro::__invokecb { id cb args } {
    variable Synchro
    variable log

    # Check that this is one of our synchronisation
    set idx [lsearch $Synchro(dirs) $id]
    if { $idx < 0 } {
	${log}::warn "Synchronisation identifier $id is not valid"
	return -code error "Synchronisation dentifier $id invalid"
    }

    set varname "::synchro::Sync_${id}"
    upvar \#0 $varname Sync

    if { $Sync($cb) != "" } {
	if { [catch {eval $Sync($cb) $id $args} res] } {
	    ${log}::warn "Error when invoking callback on $Sync(dir): $res"
	    return ""
	} else {
	    return $res
	}
    } else {
	return ""
    }
}


# ::synchro::__dosync -- Attempt one synchronisation
#
#	This command attempts to perform one synchronisation between
#	the current state on disk and a remote directory.
#
# Arguments:
#	id	Synchronisation identifier
#	rid	Source identifier within the synchronisation
#
# Results:
#	None.
#
# Side Effects:
#	Will probably trigger network operations.
proc ::synchro::__dosync { id rid } {
    variable Synchro
    variable log

    # Check that this is one of our synchronisation
    set idx [lsearch $Synchro(dirs) $id]
    if { $idx < 0 } {
	${log}::warn "Synchronisation identifier $id is not valid"
	return -code error "Synchronisation dentifier $id invalid"
    }

    set varname "::synchro::Sync_${id}"
    upvar \#0 $varname Sync

    set idx [lsearch $Sync(sources) $rid]
    if { $idx < 0 } {
	${log}::warn "Synchronisation source identifier $rid is not valid"
	return -code error "Synchronisation source identifier $rid invalid"
    }

    set varname "::synchro::Source_${id}_${rid}"
    upvar \#0 $varname Source

    ${log}::debug "Starting synchronisation from $Source(url) into $Sync(dir)"
    set start [clock clicks -milliseconds]
    set period [expr $Source(poll) * 1000]
    incr Source(nbpolls)
    __invokecb $id -startcb $Source(type) "$Source(url)" $Source(nbpolls)
    set Source(pending) ""
    if { [catch {$Source(pkg)::traverse $Source(url) \
		     $Sync(-consider) $Sync(-ignore)} flist] } {
	${log}::warn "Could not traverse source $Source(url) this time"
    } else {
	set dstdir [__dndir $id $rid]
	if { $dstdir != ""} {
	    set Source(start) $start
	    # For all traversed remote files, check their modification
	    # time against the local copy and trigger fetching if
	    # necessary.  Local directory creation and thus structure
	    # is also handled here, which means that the traversal
	    # should contained the name of sub directories before the
	    # name of their files.
	    foreach finfo $flist {
		set type [lindex $finfo 0]
		set name [lindex $finfo 1]
		set mtime [lindex $finfo 2]

		set localfname [file join $Sync(dir) $name]
		switch $type {
		    FILE {
			set lmtime 0
			if { [file exists $localfname] } {
			    if { [catch {file mtime $localfname} lmtime] } {
				${log}::warn "Could not get modification time\
                                          of $localfname: $lmtime"
				set lmtime 0
			    }
			}
			set fetch [__invokecb $id -getcb "$name" \
				       $lmtime $mtime]
			if { ($fetch == "" && ( $lmtime < $mtime \
						    || $lmtime == 0)) \
				 || ( $fetch != "" && $fetch != 0 ) } {
			    if { $fetch == "" } { set fetch -1 }
			    __fetch $id $rid $dstdir $name $fetch
			}
		    }
		    DIR {
			if { ! [file isdirectory $localfname] } {
			    if { [catch {file mkdir $localfname} err] } {
				${log}::warn \
				    "Could not make directory $localfname:\
                                     $err"
			    } else {
				if { [string is true $Sync(-fullpath)] } {
				    __invokecb $id -newcb "$localfname"
				} else {
				    __invokecb $id -newcb "$name"
				}
				${log}::info "Created directory $localfname"
			    }
			}
		    }
		}
	    }
	}
    }

    # The pending queue never contained anything, which means that we
    # did not schedule any remote URL fetching, schedule a new check
    # in a wee while.
    if { [llength $Source(pending)] == 0 } {
	__invokecb $id -endcb $Source(type) "$Source(url)" \
	    $Source(nbpolls) [expr {[clock clicks -milliseconds] - $start}]
	if { $Source(poll) >= 0 } {
	    set Source(after) [after $period ::synchro::__dosync $id $rid]
	}
    }
    return
}


# ::synchro::add -- Add a remote source
#
#	This command add a remote/local directory structure from which
#	to synchronise.  Negative polling is undertood as polling once
#	and no continuous check for file modifications and additions.
#	Polling manually at a later time for such synchronisation can
#	be achieved through calling this command again (with the same
#	type and url!) but at a later time.
#
# Arguments:
#	id	Synchronisation identifier
#	url	Location of the remote/local directory structure
#	type	Type of remote/local directory structure
#	poll	Poll period for synchronisation attempts (in seconds)
#               negative is poll once only
#
# Results:
#	An identifier for the remote source.
#
# Side Effects:
#	Will load in new package for synchronisation implementation
#	when new types are declared.
proc ::synchro::add { id url { type "" } { poll 60 } } {
    variable Synchro
    variable log

    # Check that this is one of our synchronisation
    set idx [lsearch $Synchro(dirs) $id]
    if { $idx < 0 } {
	${log}::warn "Synchronisation identifier $id is not valid"
	return -code error "Identifier invalid"
    }

    set varname "::synchro::Sync_${id}"
    upvar \#0 $varname Sync

    set rid [::md5::md5 -hex $url]
    set varname "::synchro::Source_${id}_${rid}"
    upvar \#0 $varname Source

    set idx [lsearch $Sync(sources) $rid]
    if { $idx >= 0 } {
	${log}::info "Source $url already exists in $id"
	set oldpoll $Source(poll)
	set Source(poll) $poll
	if { $oldpoll < 0 && $Source(start) != 0 } {
	    # We had a negative poll (which is understood as traverse
	    # once only) and were not performing the traversal and
	    # fetching, then schedule a new one.
	    set Source(after) [after idle ::synchro::__dosync $id $rid]
	}
    } else {
	if { $type == "" } {
	    array set top [::uri::split $url]
	    set type $top(scheme)
	    if { $top(scheme) == "http" && $top(host) == "" } {
		set type "file"
	    }
	    ${log}::notice "Guessing that $url is of type: $type"
	}

	if { [catch {package require synchro::$type} ver] } {
	    ${log}::warn \
		"Could not find synchronsation implementation for $type: $ver"
	    return -code error \
		"Could not find sync implementation package for $type"
	} else {
	    ${log}::debug \
		"Found sync implementation for $type at version \# $ver"
	    set Source(id) $rid
	    set Source(sid) $id
	    set Source(type) $type
	    set Source(pkg) ::synchro::$type
	    set Source(url) $url
	    set Source(poll) $poll
	    set Source(after) [after idle ::synchro::__dosync $id $rid]
	    set Source(start) 0
	    set Source(nbpolls) 0

	    lappend Sync(sources) $rid

	    lappend Synchro(packages) $Source(pkg)
	    set Synchro(packages) [lsort -unique $Synchro(packages)]

	    $Source(pkg)::loglevel $Synchro(loglevel)
	}
    }

    return $Source(id)
}



# ::synchro::remove -- Remove a source from a synchronisation
#
#	This command will remove a source from a synchronisatio.  All
#	pending synchronisations will be cancelled and have no
#	effect.
#
# Arguments:
#	id	Synchronisation identifier
#	rid	Source identifier within the synchronisation
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::synchro::remove { id rid } {
    variable Synchro
    variable log

    # Check that this is one of our synchronisation
    set idx [lsearch $Synchro(dirs) $id]
    if { $idx < 0 } {
	${log}::warn "Synchronisation identifier $id is not valid"
	return -code error "Synchronisation dentifier $id invalid"
    }

    set varname "::synchro::Sync_${id}"
    upvar \#0 $varname Sync

    set idx [lsearch $Sync(sources) $rid]
    if { $idx < 0 } {
	${log}::warn "Synchronisation source identifier $rid is not valid"
	return -code error "Synchronisation source identifier $rid invalid"
    }

    set varname "::synchro::Source_${id}_${rid}"
    upvar \#0 $varname Source

    after cancel $Source(after)
    set Sync(sources) [lreplace $Sync(sources) $idx $idx]
}


# ::synchro::delete -- Delete a synchronisation
#
#	This command will delete a synchronisation and all its
#	sources.  All pending synchronisations will be cancelled and
#	have no effects.
#
# Arguments:
#	id	Synchronisation identifier
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::synchro::delete { id } {
    variable Synchro
    variable log

    # Check that this is one of our synchronisation
    set idx [lsearch $Synchro(dirs) $id]
    if { $idx < 0 } {
	${log}::warn "Synchronisation identifier $id is not valid"
	return -code error "Identifier invalid"
    }

    set varname "::synchro::Sync_${id}"
    upvar \#0 $varname Sync
    
    foreach rid $Sync(sources) {
	remove $id $rid
    }

    set cleanptn $Sync(-dndir_pfx)
    append cleanptn "*"
    append cleanptn $Sync(-dndir_ext)
    set nbcleaned [::diskutil::clean_directory $Sync(dir) $cleanptn]
    ${log}::info "Cleaned $nbcleaned old temporary directories"

    set Synchro(dirs) [lreplace $Synchro(dirs) $idx $idx]
}


# ::synchro::new -- Create a synchronisation
#
#	This command creates a new synchronisation for an (existing)
#	directory.  The configuration arguments are passed further to
#	config.  Several different synchronisation (contexts) can
#	exist for the same directory, if that is wished (and even
#	though it is a bit awkward)
#
# Arguments:
#	dir	Local directory to host synchronisation
#	cb	Callback to call on new/modified files.
#	args	list of options
#
# Results:
#	Return an identifier for the synchronisation
#
# Side Effects:
#	None.
proc ::synchro::new { dir args } {
    variable Synchro
    variable log

    set id [incr Synchro(idgene)]

    # Create information for the synchronisation
    set varname "::synchro::Sync_${id}"
    upvar \#0 $varname Sync

    set Sync(id) $id
    set Sync(dir) $dir
    set Sync(sources) ""
    foreach opt [array names Synchro "-*"] {
	set Sync($opt) $Synchro($opt)
    }
    lappend Synchro(dirs) $id

    eval config $id $args

    set cleanptn $Sync(-dndir_pfx)
    append cleanptn "*"
    append cleanptn $Sync(-dndir_ext)
    set nbcleaned [::diskutil::clean_directory $Sync(dir) $cleanptn]
    ${log}::info "Cleaned $nbcleaned old temporary directories"

    return $id
}


# ::synchro::config -- Configure a synchronisation
#
#	This command set or get the options of a synchronisation.
#
# Arguments:
#	id	Synchronisation identifier
#	args	list of options
#
# Results:
#	Return all options, the option requested or set the options
#
# Side Effects:
#	None.
proc ::synchro::config { id args } {
    variable Synchro
    variable log

    # Check that this is one of our connections
    set idx [lsearch $Synchro(dirs) $id]
    if { $idx < 0 } {
	${log}::warn "Synchronisation identifier $id is not valid"
	return -code error "Identifier invalid"
    }

    set varname "::synchro::Sync_${id}"
    upvar \#0 $varname Sync

    set o [lsort [array names Sync "-*"]]

    if { [llength $args] == 0 } {      ;# Return all results
	set result ""
	foreach name $o {
	    lappend result $name $Sync($name)
	}
	return $result
    }

    foreach {opt value} $args {        ;# Get one or set some
	if { [lsearch $o $opt] == -1 } {
	    return -code error "Unknown option $opt, must be: [join $o ", " ]"
	}
	if { [llength $args] == 1 } {  ;# Get one config value
	    return $Sync($opt)
	}
	set Sync($opt) $value         ;# Set the config value
    }
}



# ::synchro::defaults -- Set/Get defaults for all new synchronisations
#
#	This command sets or gets the defaults opetions for all new
#	synchronisations, it will not perpetrate on existing
#	synchronisations, use ::synchro::config instead.
#
# Arguments:
#	args	List of -key value or just -key to get value
#
# Results:
#	Return all options, the option requested or set the options
#
# Side Effects:
#	None.
proc ::synchro::defaults { args } {
    variable Synchro
    variable log

    set o [lsort [array names Synchro "-*"]]

    if { [llength $args] == 0 } {      ;# Return all results
	set result ""
	foreach name $o {
	    lappend result $name $Synchro($name)
	}
	return $result
    }

    foreach {opt value} $args {        ;# Get one or set some
	if { [lsearch $o $opt] == -1 } {
	    return -code error "Unknown option $opt, must be: [join $o ,]"
	}
	if { [llength $args] == 1 } {  ;# Get one config value
	    return $Synchro($opt)
	}
	set Synchro($opt) $value           ;# Set the config value
    }
}
