# spool.tcl --
#
#	This module contains commands to treat existing directories as
#	spool directories.  Writing and reading from the spool is
#	typically separated.  Reading is done in an asynchronous
#	manner and through callbacks.  A spool is a directory that
#	contains three sub-directories with adequate semantics: inbox,
#	error and sent.  inbox is where file arrive, sent where files
#	that have been treated are moved and error where files that
#	failed to be treated are moved.
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

package require Tcl 8.2
package require logger
package require diskutil 1.5

package provide spool 1.0

namespace eval ::spool {
    # Variables of name ::spool::__Spool_<id> are created as
    # arrays to support each spool.

    # initialise the global state
    variable SpoolState
    if {![info exists SpoolState]} {
	array set SpoolState {
	    gene_idx       0
	    directories    ""
	    inbox          "inbox"
	    sent           "sent"
	    error          "error"
	    dft_rootdir    "%progdir%/spoolers/%progname%"
	    rootdir        ""
	    loglevel       warn
	}
	variable log [::logger::init spool]
	${log}::setlevel $SpoolState(loglevel)
    }

    # Export library commands
    namespace export inbox rootdir directory_generate new delete lock unlock
    namespace export write cancel ioctl check
}



# ::spool::loglevel -- Set/Get current log level.
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
proc ::spool::loglevel { { loglvl "" } } {
    variable SpoolState
    variable log

    if { $loglvl != "" } {
	if { [catch "${log}::setlevel $loglvl"] == 0 } {
	    set SpoolState(loglevel) $loglvl
	}
    }

    return $SpoolState(loglevel)
}


# ::spool::find --
#
#	Find a spool directory (with associated pattern)
#
# Arguments:
#	dir	Main spool directory
#	ptn	Patterns for files to watch in directory.
#
# Results:
#	Return the identifier of the spooler(or an empty string if not
#	found).
#
# Side Effects:
#	None.
proc ::spool::find { dir { ptn * } } {
    variable SpoolState

    foreach d $SpoolState(directories) {
	upvar \#0 $d Spool
	if { $Spool(directory) == $dir && $Spool(file_ptn) == $ptn } {
	    return $d
	}
    }

    return ""
}


# ::spool::empty --
#
#	Send all the files that are currently in the pool to the
#	callbacks registered for that directories.  Returns a list of
#	the files that were actually sent further and thus removed
#	from the inbox. Deletion will occur as soon as one of the
#	callbacks returns, but all callbacks are guaranteed to be
#	called with the name of a file that exists.  As soon as one
#	callback returns ERROR, the file will be moved to the error
#	directory.  As soon as one callback returns SENT, the file
#	will be moved to the sent directory.  As soon as one callback
#	returns KEEP, the file will stay in the inbox and re-treated
#	next time.  Any other return value will lead to the file being
#	removed from the inbox directory (as opposed to moved!).
#
# Arguments:
#	spool	Identifier (variable) containing spool information
#
# Results:
#	Returns the list of files that were treated and moved to other
#	parts of the spooler.
#
# Side Effects:
#	This commands actively moves or removes file on the disk
proc ::spool::empty { spool } {
    variable SpoolState
    variable log

    upvar \#0 $spool Spool
    set flushed ""
    
    # Get the current spooled files from the directory. We do this
    # once and will only work on this copy of the file list.
    # Consequently, if the command to send a message below takes time
    # and if files are added to the spool, these will only be detected
    # the next time we get into this command, which is the required
    # behaviour!
    if { [catch "glob -directory [::spool::inbox $spool] $Spool(file_ptn)" \
	      files] == 0 } {
	${log}::notice \
	    "[llength $files] files to be removed from\
             [::spool::inbox $spool]/$Spool(file_ptn) directory"

	# For each file present in the spool at the time of checking, read
	# its content and forward it to the callbacks.
	foreach fname $files {
	    # Deliver callbacks
	    set directives ""
	    foreach cb $Spool(cbs) {
		lappend directives [eval $cb $spool {$fname}]
	    }

	    # Analyse answer and move/delete file accordingly.
	    set err [lsearch $directives "ERROR"]
	    if { $err >= 0 } {
		eval $Spool(move_cmd) {$fname} \
		    [file join $Spool(directory) $SpoolState(error)]
		${log}::info \
		    "[file tail $fname] moved to $SpoolState(error)\
                    subdirectory"
		lappend flushed $fname
	    } else {
		set sent [lsearch $directives "SENT"]
		if { $sent >= 0 } {
		    eval $Spool(move_cmd) {$fname} \
			[file join $Spool(directory) $SpoolState(sent)]
		    ${log}::info \
			"[file tail $fname] moved to $SpoolState(sent)\
                         subdirectory"
		    lappend flushed $fname
		} else {
		    set keep [lsearch $directives "KEEP"]
		    if { $keep < 0 } {
			eval $Spool(delete_cmd) {$fname}
			lappend flushed $fname
			${log}::info \
			    "[file tail $fname] removed from\
                             $SpoolState(inbox) subdirectory"
		    } else {
			${log}::info \
			    "[file tail $fname] kept in\
                             $SpoolState(inbox) subdirectory"
		    }
		}
	    }
	}
    } else {
	${log}::debug \
	    "Spool directory [::spool::inbox $spool]/$Spool(file_ptn) is empty"
    }

    return $flushed
}



# ::spool::periodcheck --
#
#	This command periodically checks the spool directory and empty
#	its content, i.e. forwards all the file that it contains to
#	the registerd callbacks.  Access to the spool directory is
#	locked through the locked variable index.
#
# Arguments:
#	spool	Identifier (variable) containing spool information
#
# Results:
#	None.
#
# Side Effects:
#	Through calling check, this command might move or remove files
#	on disk
proc ::spool::periodcheck { spool } {
    upvar \#0 $spool Spool

    ::spool::check $spool
    set Spool(after_id) [after $Spool(period) "::spool::periodcheck $spool"]
}



# ::spool::inbox --
#
#	Return the name of the inbox subdirectory.  This can be used
#	to place files in the spooler from external programs, in which
#	case the spooler should be locked until the file is created,
#	closed and ready for treatment.
#
# Arguments:
#	spool	Identifier of spooler, as returned by ::spool::new
#
# Results:
#	Return the path to the inbox.
#
# Side Effects:
#	None.
proc ::spool::inbox { spool } {
    variable SpoolState

    upvar \#0 $spool Spool
    return [file join $Spool(directory) $SpoolState(inbox)]
}


# ::spool::rootdir --
#
#	Set/Get root directory for all spoolers.
#
# Arguments:
#	rootdir	New rootdir
#
# Results:
#	Return the path to the current root directory of all spoolers.
#
# Side Effects:
#	None.
proc ::spool::rootdir { { rootdir "" } } {
    variable SpoolState

    if { $rootdir == "" } {
	if { $SpoolState(rootdir) == "" } {
	    set SpoolState(rootdir) \
		[::diskutil::fname_resolv $SpoolState(dft_rootdir)]
	}
    } else {
	set SpoolState(rootdir) $rootdir
    }

    return $SpoolState(rootdir)
}


# ::spool::directory_generate --
#
#	Generate the name of a suitable directory for spooling
#
# Arguments:
#	args	Each additionaly keyword will lead to subdirectories.
#
# Results:
#	Return the name of a directory that can be user for spooling.
#
# Side Effects:
#	None.
proc ::spool::directory_generate { args } {
    set dir [::spool::rootdir]
    foreach d $args {
	set dir [file join $dir $d]
    }

    return $dir
}



# ::spool::new --
#
#	Create a new spool/queue directory and start watching this
#	directory for new files.
#
# Arguments:
#	cb	Add a callback function that will be called for each new
#               arriving file.
#		descr
#	ptn	Pattern that each file should match to be treated.
#	dir	Main directory (sub-directories will be created).  A
#               suitable directory is guessed from the default root
#               directory if empty (see rootdir).
#	period	Period at which to watch directory, in ms
#
# Results:
#	Return an identifier to the created spool or an empty string.
#
# Side Effects:
#	None.
proc ::spool::new { cb { ptn * } { dir "" } { period 500 } } {
    variable SpoolState
    variable log

    # Guess a good directory for dir if not specified!
    if { $dir == "" } {
	set dir [::spool::directory_generate]
    }

    set d [::spool::find $dir $ptn]
    if { $d == "" } {
	incr SpoolState(gene_idx)
	set varname "__Spool_$SpoolState(gene_idx)"
	upvar \#0 $varname Spool

	set inbox_dir [file join $dir $SpoolState(inbox)]
	if { [catch "file mkdir $inbox_dir"] != 0 } {
	    ${log}::warn "Cannot create spool inbox directory $inbox_dir"
	    return ""
	}
	set error_dir [file join $dir $SpoolState(error)]
	if { [catch "file mkdir $error_dir"] != 0 } {
	    ${log}::warn "Cannot create spool error directory $error_dir"
	    return ""
	}
	set sent_dir [file join $dir $SpoolState(sent)]
	if { [catch "file mkdir $sent_dir"] != 0 } {
	    ${log}::warn "Cannot create sent spool directory $sent_dir"
	    return ""
	}

	set Spool(directory) $dir
	set Spool(id) $SpoolState(gene_idx)
	set Spool(cbs) $cb
	set Spool(file_ptn) $ptn
	set Spool(period) $period
	set Spool(lock) 0
	set Spool(after_id) \
	    [after $Spool(period) ::spool::periodcheck $varname]
	set Spool(move_cmd) "file rename -force --"
	set Spool(delete_cmd) "file delete -force --"
	return $varname
    } else {
	upvar \#0 $d Spool
	lappend Spool(cbs) $cb
	return $d
    }
}



# ::spool::delete --
#
#	Remove a spooler object.  Do not remove any pending files.
#
# Arguments:
#	spool	Identifier of spooler, as returned by ::spool::new
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::spool::delete { spool } {
    variable SpoolState

    set idx [lsearch $SpoolState(directories) $spool]
    if { $idx > 0 } {
	set SpoolState(directories) \
	    [lreplace $SpoolState(directories) $idx $idx]
	upvar \#0 $spool Spool
	after cancel $Spool(after_id)
	unset Spool
    }
}


# ::spool::lock --
#
#	Lock spool directory to start putting files in it.
#
# Arguments:
#	spool	Identifier of spooler, as returned by ::spool::new
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::spool::lock { spool } {
    upvar \#0 $spool Spool
    incr Spool(lock)
}


# ::spool::unlock --
#
#	Unlock spool directory to start putting files in it.
#
# Arguments:
#	spool	Identifier of spooler, as returned by ::spool::new
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::spool::unlock { spool } {
    upvar \#0 $spool Spool
    incr Spool(lock) -1
}



# ::spool::write --
#
#	Move one file into the inbox queue.  Use the ioctl functions
#	that have been setup for that purpose.
#
# Arguments:
#	spool	Identifier of spooler, as returned by ::spool::new
#	fname	Name of file to be moved into the spooler for treatment
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::spool::write { spool fname } {
    upvar \#0 $spool Spool
    set fullpath [file join [::spool::inbox $spool] [file tail $fname]]
    ::spool::lock $spool
    eval $Spool(move_cmd) {$fname} [::spool::inbox $spool]
    ::spool::unlock $spool

    return $fullpath
}



# ::spool::cancel --
#
#	Remove a given file from the spooler's inbox.  Use the ioctl
#	functions that have been setup for that purpose.
#
# Arguments:
#	spool	Identifier of spooler, as returned by ::spool::new
#	fname	Name of file to be removed from the spooler
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::spool::cancel { spool fname } {
    upvar \#0 $spool Spool
    set fullpath [file join [::spool::inbox $spool] [file tail $fname]]
    ::spool::lock $spool
    eval $Spool(delete_cmd) {$fullpath}
    ::spool::unlock $spool

    return $fullpath
}



# ::spool::ioctl --
#
#	Specify different commands to move and delete files.  These
#	functions can for example be used to move or delete groups of
#	files whenever one file has been treated.
#
# Arguments:
#	spool	Identifier of spooler, as returned by ::spool::new
#	move_cmd	Command to be called when moving a file.  First
#                       argument will be the name of the file to move
#                       and second argument the destination directory.
#	delete_cmd	Command to be called when deleting a file.  Only
#                       argument will be the name of the file to delete.
#
# Results:
#	Return a list with two elements one for the old move comand
#	and one for the old delete command
#
# Side Effects:
#	None.
proc ::spool::ioctl { spool { move_cmd "" } { delete_cmd "" } } {
    upvar \#0 $spool Spool

    set previous [list $Spool(move_cmd) $Spool(delete_cmd)]
    if { $move_cmd != "" } {
	set Spool(move_cmd) $move_cmd
    }
    if { $delete_cmd != "" } {
	set Spool(delete_cmd) $delete_cmd
    }

    return $previous
}



# ::spool::check --
#
#	Check the spool content once and take the appropriate actions
#	if it is not locked.  "Actions" here means that the callbacks
#	generated when new files have arrived will be generated and
#	that these files will be moved in consequence.  
#
# Arguments:
#	spool 	Identifier of spooler, as returned by ::spool::new
#
# Results:
#	Return the number of files treated or a negative number if the
#	spool was currently locked and nothing could be done.
#
# Side Effects:
#	None.
proc ::spool::check { spool } {
    variable log

    upvar \#0 $spool Spool

    set nbfiles -1
    if { $Spool(lock) <= 0 } {
	set files [::spool::empty $spool]
	set nbfiles [llength $files]
	if { $nbfiles > 0 } {
	    ${log}::info \
		"Removed and forwarded $nbfiles file(s) from\
                 spool [::spool::inbox $spool]/$Spool(file_ptn)"
	}
    }
    
    return $nbfiles
}
