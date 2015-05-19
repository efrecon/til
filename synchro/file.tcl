# file.tcl --
#
#	This modules provides the local file synchronisation
#	implementation.
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

package require Tcl 8.3
package require logger
package require diskutil

package provide synchro::file 1.1

namespace eval ::synchro::file {
    variable File
    if {![::info exists File]} {
	array set File {
	    idgene        0
	    pending       ""
	    loglevel      warn
	    pgess_period  500
	}
	variable log [::logger::init [string trimleft [namespace current] ::]]
	${log}::setlevel $File(loglevel)
    }

    namespace export loglevel traverse geturl cancel
}


# ::synchro::file::loglevel -- Set/Get current log level.
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
proc ::synchro::file::loglevel { { loglvl "" } } {
    variable File
    variable log

    if { $loglvl != "" } {
	if { [catch "${log}::setlevel $loglvl"] == 0 } {
	    set File(loglevel) $loglvl
	}
    }

    return $File(loglevel)
}


# ::synchro::file::__traverse -- Traverse directory structure
#
#	Traverse directory structure and gather information about the
#	files and directories traversed.  See to remove the top
#	traversal directory from the file names being stored so that
#	all names are relative.
#
# Arguments:
#	dirtop	Directory to traverse
#	strip	Directory information to strip, i.e. origin top directory
#	consider	List of regular expressions for file names to consider
#	ignore	List of regular expressions for file names to ignore
#
# Results:
#	Return the list of (relative) directories and files.
#
# Side Effects:
#	None.
proc ::synchro::file::__traverse { dirtop strip {consider {".*"}} {ignore {}} } {
    variable File
    variable log

    # Get all readable files from the directory using a 8.3 compatible command
    set flist [glob -directory $dirtop -nocomplain -types r -- *]
    ${log}::debug "Entering $dirtop"

    # Add information about all files to the result list.
    set reslist ""
    foreach fname $flist {
	# Strip away top directory
	set idx [string first $strip $fname]
	if { $idx != 0 } {
	    ${log}::warn "Could not find $strip in recursed file name $fname!"
	} else {
	    set rname [string range $fname [string length $strip] end]
	    
	    # Recurse into directories and make sure to
	    # create information with the type of the path, the
	    # relative name and its modification time.
	    # Check whether we should count on that file or not.
	    if { [::diskutil::match $rname $consider $ignore] } {
		if { [file isdirectory $fname] } {
		    lappend reslist [list DIR $rname [file mtime $fname]]
		    foreach sub [__traverse $fname $strip $consider $ignore] {
			lappend reslist $sub
		    }
		} else {
		    lappend reslist [list FILE $rname [file mtime $fname]]
		}
	    }
	}
    }

    return $reslist
}


# ::synchro::file::traverse -- Traverse directory structure
#
#	Traverse directory structure.
#
# Arguments:
#	dirtop	Top directory to traverse
#	consider	List of regular expressions for file names to consider
#	ignore	List of regular expressions for file names to ignore
#
# Results:
#	Return the list of (relative) directories and files.
#
# Side Effects:
#	None.
proc ::synchro::file::traverse { dirtop { consider {".*"} } { ignore {} } } {
    variable File
    variable log

    if { ! [file isdirectory $dirtop] } {
	return -code error "$dirtop is not a directory"
    }
    if { ! [file readable $dirtop] } {
	return -code error "$dirtop is not readable"
    }

    if { [string index $dirtop end] != "/" } {
	append dirtop "/"
    }

    return [__traverse $dirtop $dirtop $consider $ignore]
}


# ::synchro::file::__invokecb -- Invoke a copy done callback
#
#	Invoke a copy done callback in a manner that will catch errors
#	nicely.
#
# Arguments:
#	cb	Command to callback
#	src	Source file name
#	dst	Destination file name
#	res	Result
#	txt	Possible additional text
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::synchro::file::__invokecb { cb src dst res { txt "" } } {
    variable File
    variable log

    if { $cb != "" } {
	if { [catch {eval $cb \$src \$dst $res \$txt} err] } {
	    ${log}::warn "Error when invoking callback on $src: $err"
	}
    }
}


# ::synchro::file::__donecp -- Copy completed
#
#	Command called back once a file has been copied or has failed
#	to do so.  Analyse the result and tell all relevent callers.
#
# Arguments:
#	id	Identifier of copy
#	bytes	Number of bytes successfully copied
#	err	Possible error.
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::synchro::file::__donecp { id bytes {err {}}} {
    variable File
    variable log

    set idx [lsearch $File(pending) $id]
    if { $idx < 0 } {
	${log}::warn "$id does not identify a file copy anymore!!"
	return
    }
    
    set varname "::synchro::file::Copy_${id}"
    upvar \#0 $varname Copy

    if { $Copy(cancelled) } {
	${log}::debug "Copy from $Copy(srcfname) into $Copy(dstfname)\
                       cancelled: $bytes bytes were done"
	foreach cb $Copy(done_cbs) {
	    __invokecb $cb $Copy(srcfname) $Copy(dstfname) "CANCEL"
	}
	if { [file exists $Copy(dstfname)] \
		 && [catch {file delete $Copy(dstfname)} err] } {
	    ${log}::warn \
		"Could not delete incomplete destination file \
                 '$Copy(dstfname)': $err"
	}
    } else {
	catch {close $Copy(src_fdes)}
	catch {close $Copy(dst_fdes)}

	if { [string length $err] == 0 } {
	    ${log}::debug \
		"Copied $bytes bytes from $Copy(srcfname) into $Copy(dstfname)"
	    foreach cb $Copy(done_cbs) {
		__invokecb $cb $Copy(srcfname) $Copy(dstfname) "OK"
	    }
	} else {
	    ${log}::warn \
		"Could not copy $Copy(srcfname) into $Copy(dstfname)"
	    foreach cb $Copy(done_cbs) {
		__invokecb $cb $Copy(srcfname) $Copy(dstfname) "ERROR" $err
	    }
	    if { [catch {file delete $Copy(dstfname)} err] } {
		${log}::warn \
		    "Could not delete incomplete destination file \
                     '$Copy(dstfname)': $err"
	    }
	}
    }

    unset Copy
    set File(pending) [lreplace $File(pending) $idx $idx]

    return
}


# ::synchro::file::cancel -- Cancel a copy
#
#	This command cancels a copy in progress, if any and possible.
#	It will call the callbacks with "CANCEL" as an argument.
#
# Arguments:
#	id	Identifier of copy
#
# Results:
#	Boolean telling whether we could cancel the copy or not.
#
# Side Effects:
#	Will remove destination file.
proc ::synchro::file::cancel { id } {
    variable File
    variable log

    set idx [lsearch $File(pending) $id]
    if { $idx < 0 } {
	${log}::warn "$id does not identify a file copy anymore!!"
	return
    }
    
    set varname "::synchro::file::Copy_${id}"
    upvar \#0 $varname Copy

    # Close the file descriptor and remember state in Copy structure,
    # the remaining of the work will be done when __donecb is called.
    catch {close $Copy(src_fdes)}
    catch {close $Copy(dst_fdes)}

    set Copy(cancelled) 1
}


# ::synchro::file::__progressreport -- Report progress
#
#	Command called back once in a while to report progress back to
#	the caller.
#
# Arguments:
#	id	Identifier of copy
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::synchro::file::__progressreport { id } {
    variable File
    variable log

    # Not a copy anymore, return silently and stop checking for
    # progress.
    set idx [lsearch $File(pending) $id]
    if { $idx < 0 } {
	return
    }
    
    set varname "::synchro::file::Copy_${id}"
    upvar \#0 $varname Copy

    foreach cb $Copy(pgess_cbs) {
	if { [catch {eval $cb $id $Copy(dstfname)} err] } {
	    ${log}::warn "Error when invoking progress callback on $id: $err"
	}
    }

    after $File(pgess_period) ::synchro::file::__progressreport $id
}


# ::synchro::file::geturl -- Copy a file
#
#	This command will copy a file from another directory into a
#	destination file name.  A callback will be invoked once the
#	copy has ended or has failed.  The callback will be callbed
#	with the following arguments: the full name of the source
#	file, the path to the destination file, a status code (OK or
#	ERROR) and, in case of errors, some additional text explaining
#	the error.  This command perform the copy in the background
#	and is therefor able to handle several calls with the same
#	source and destinations gracefully (i.e. through only copying
#	once!).  The "stepcb" is a command with which the caller can
#	interact with the copy during its progression.  For long copy
#	operations, the command will be called, providing as such a
#	progress report.  The command is then able to cancel a copy in
#	progress, if that was necessary.  It takes the identifier of
#	the copy (as return by ::geturl) and the path to the
#	destination as arguments.
#
# Arguments:
#	dirtop	Top directory under which the source file is.
#	src	Relative path to the source file.
#	dst_fp	Name of destination file.
#	donecb	Command to call back at completion.
#	pgesscb	Command to call back for progress report interaction
#	bytes	How many bytes to copy from file?
#
# Results:
#	An identifier for the copy operation or empty string on error
#
# Side Effects:
#	Will copy file on the local hard drive!
proc ::synchro::file::geturl { dirtop src dst_fp donecb {pgesscb ""} { bytes -1}} {
    variable File
    variable log

    set src_fp [file join $dirtop $src]

    # Append the callback to any existing pending copy
    foreach id $File(pending) {
	set varname "::synchro::file::Copy_${id}"
	upvar \#0 $varname Copy

	if { $Copy(srcfname) == $src_fp && $Copy(dstfname) == $dst_fp } {
	    lappend Copy(done_cbs) $donecb
	    if { $pgesscb != "" } {
		lappend Copy(pgess_cbs) $pgesscb
		if { [llength $Copy(pgess_cbs)] == 1 } {
		    after $File(pgess_period) \
			::synchro::file::__progressreport $Copy(id)
		}
	    }
	    return $Copy(id)
	}
    }

    # Now check that we can open the source and destination, invoke
    # the callback with an error if we could not, and return.
    if { ! [file exists $src_fp] || ! [file readable $src_fp] } {
	__invokecb $donecb $src_fp $dst_fp "ERROR" \
	    "Cannot access source file: $src_fp"
	return ""
    }

    if { [catch {open $src_fp} src_fdes] } {
	__invokecb $donecb $src_fp $dst_fp "ERROR" $src_fdes
	return ""
    }
    if { [catch {open $dst_fp w} dst_fdes] } {
	__invokecb $donecb $src_fp $dst_fp "ERROR" $dst_fdes
	return ""
    }

    # Otherwise, store copying information and do the file copying in
    # the background.
    set id [incr File(idgene)]
    set varname "::synchro::file::Copy_${id}"
    upvar \#0 $varname Copy

    set Copy(id) $id
    set Copy(srcfname) $src_fp
    set Copy(dstfname) $dst_fp
    set Copy(src_fdes) $src_fdes
    set Copy(dst_fdes) $dst_fdes
    set Copy(done_cbs) [list $donecb]
    set Copy(cancelled) 0
    if { $pgesscb == "" } {
	set Copy(pgess_cbs) {}
    } else {
	set Copy(pgess_cbs) [list $pgesscb]
	after $File(pgess_period) ::synchro::file::__progressreport $id
    }
    lappend File(pending) $id

    # Start the background copy
    ${log}::debug "Copying $src from $dirtop, into $dst_fp..."
    fconfigure $src_fdes -translation binary -encoding binary
    fconfigure $dst_fdes -translation binary -encoding binary
    set cpycmd \
	[list fcopy $src_fdes $dst_fdes \
	     -command [list ::synchro::file::__donecp $id]]
    if { $bytes >= 0 } {
	lappend cpycmd -size $bytes
    }
    eval $cpycmd

    return $id
}
