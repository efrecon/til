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
package require ftp
package require uri

package provide synchro::ftp 1.1

namespace eval ::synchro::ftp {
    variable Ftp
    if {![::info exists Ftp]} {
	array set Ftp {
	    idgene        0
	    default_port  21
	    pending       ""
	    queues        ""
	    poll_max      2000
	    loglevel      warn
	    equalstop     10000
	    onerr_retry   5000
	    follow_links  1
	}
	variable log [::logger::init [string trimleft [namespace current] ::]]
	${log}::setlevel $Ftp(loglevel)
    }

    namespace export loglevel traverse geturl cancel
}


# ::synchro::ftp::loglevel -- Set/Get current log level.
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
proc ::synchro::ftp::loglevel { { loglvl "" } } {
    variable Ftp
    variable log

    if { $loglvl != "" } {
	if { [catch "${log}::setlevel $loglvl"] == 0 } {
	    set Ftp(loglevel) $loglvl
	}
    }

    return $Ftp(loglevel)
}


# ::synchro::ftp::__parse_ls -- Parse an ls -l line
#
#	Parse a line from ls -l and return data.  This code is
#	borrowed and adapted from the Tcl library.
#
# Arguments:
#	line	Line to parse
#
# Results:
#	Return a dictionary ready for array set.
#
# Side Effects:
#	None.
proc ::synchro::ftp::__parse_ls { line } {
    variable Ftp
    variable log

    foreach {mode dummy owner group size month day yrtime name} $line break
    if {[string first : $yrtime] >=0} {
	set date \
	    "$day $month [clock format [clock seconds] -format %Y] $yrtime"
    } else {
	set date "$day $month $yrtime 00:00"
    }
    set info [list owner $owner group $group size $size date $date]
    
    switch -exact -- [string index $mode 0] {
	- {set type file}
	d {set type dir}
	l {set type link ; lappend info link [lindex $line end]}
    }
    lappend info name $name
    lappend info type $type
    
    return $info
}


# ::synchro::ftp::__traverse -- Traverse directory structure
#
#	Traverse directory structure and gather information about the
#	files and directories traversed.  See to remove the top
#	traversal directory from the file names being stored so that
#	all names are relative.
#
# Arguments:
#	dirtop	FTP Directory to traverse
#	strip	Directory information to strip, i.e. origin top directory
#	consider	List of regular expressions for file names to consider
#	ignore	List of regular expressions for file names to ignore
#
# Results:
#	Return the list of (relative) directories and files.
#
# Side Effects:
#	None.
proc ::synchro::ftp::__traverse { ftpd dirtop strip {consider {".*"}} {ignore {}} } {
    variable Ftp
    variable log

    ${log}::debug "Entering $dirtop"
    set flist [::ftp::List $ftpd $dirtop]

    # Add information about all files to the result list.
    set reslist ""
    foreach finfo $flist {
	array set fdescr [__parse_ls $finfo]

	set fullpath $dirtop
	if { [string index $fullpath end] != "/" } {
	    append fullpath "/"
	}
	append fullpath $fdescr(name)
	set rname [string range $fullpath [string length $strip] end]

	switch $fdescr(type) {
	    "dir" {
		if { [catch {clock scan $fdescr(date) -gmt on} mtime] } {
		    ${log}::warn "Could not understand $fdescr(date) as a date"
		    set mtime [clock seconds]
		}
		if { [::diskutil::match $rname $consider $ignore] } {
		    lappend reslist [list DIR $rname $mtime]
		    foreach sub [__traverse $ftpd $fullpath $strip \
				     $consider $ignore] {
			lappend reslist $sub
		    }
		}
	    }
	    "link" {
		if { $Ftp(follow_links) } {
		    ${log}::debug \
			"Following link from $fullpath to $fdescr(link)"
		    if { [::diskutil::match $rname $consider $ignore] } {
			if { [__isfile $ftpd $fullpath] } {
			    set mtime [::ftp::ModTime $ftpd $fullpath]
			    lappend reslist [list FILE $rname $mtime]
			} else {
			    set dstdir [file join $dirtop $fdescr(link)]
			    set dstdir [::diskutil::normalize $dstdir 1]
			    if { [string index $dstdir end] != "/" } {
				append dstdir "/"
			    }
			    
			    set subs [__traverse $ftpd $dstdir $dstdir \
					  $consider $ignore]
			    
			    lappend reslist [list DIR $rname $mtime]
			    foreach sinfo $subs {
				set sname [lindex $sinfo 1]
				set sinfo [lreplace $sinfo 1 1 \
					       "${rname}/${sname}"]
				lappend reslist $sinfo
			    }
			}
		    }
		}
	    }
	    default {
		# Check whether we should count on that file or not.
		if { [::diskutil::match $rname $consider $ignore] } {
		    set mtime [::ftp::ModTime $ftpd $fullpath]
		    lappend reslist [list FILE $rname $mtime]
		}
	    }
	}
    }

    return $reslist
}


# ::synchro::ftp::__isfile -- Decides if a path points to a file
#
#	Actively get information about a file or directory
#
# Arguments:
#	ftpd	Identifier of ftp connection
#	path	Path to test.
#
# Results:
#	Return 1 if the path points to a file, 0 otherwise.
#
# Side Effects:
#	Actively uses the FTP connection.
proc ::synchro::ftp::__isfile { ftpd path } {
    variable Ftp
    variable log

    set flist [::ftp::NList $ftpd $path]
    if { $flist == "" } {
	return -code error "$path does not exist"
    }
    return [string equal $flist $path]
}


# ::synchro::ftp::traverse -- Traverse FTP directory structure
#
#	Traverse FTP directory structure.
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
proc ::synchro::ftp::traverse { dirtop { consider {".*"} } { ignore {} } } {
    variable Ftp
    variable log
    
    array set top [uri::split $dirtop]
    if { $top(port) == "" } {
	set ftpd [::ftp::Open $top(host) $top(user) $top(pwd)]
    } else {
	set ftpd [::ftp::Open $top(host) $top(user) $top(pwd) -port $top(port)]
    }
    if { $ftpd < 0 } {
	return -code error "Could not open FTP connection to $top(host)"
    }

    # Check that topdirectory is a directory
    set topdir $top(path)
    if { [__isfile $ftpd $topdir] } {
	return -code error "$dirtop points to a single file"
    }
    if { [string index $topdir end] != "/" } {
	append topdir "/"
    }

    ::ftp::Type $ftpd binary
    set flist [__traverse $ftpd $topdir $topdir $consider $ignore]
    ::ftp::Close $ftpd

    return $flist
}


# ::synchro::ftp::__invokecb -- Invoke a copy done callback
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
proc ::synchro::ftp::__invokecb { cb src dst res { txt "" } } {
    variable Ftp
    variable log

    if { $cb != "" } {
	if { [catch {eval $cb \$src \$dst $res \$txt} err] } {
	    ${log}::warn "Error when invoking callback on $src: $err"
	}
    }
}


# ::synchro::ftp::__opcb -- Execute operation callbacks
#
#	This procedure will callback all the commands that have been
#	associated to an occurring operation.
#
# Arguments:
#	host	Host name of FTP server
#	port	Port of FTP server
#	op	Operation that occurred
#	args	Additional arguments to operation
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::synchro::ftp::__opcb { host port op args } {
    variable Ftp
    variable log

    if { $port == "" } { set port $Ftp(default_port) }
    set queue "${host}_${port}"
    set varname "::synchro::ftp::Queue_${queue}"
    upvar \#0 $varname Queue

    # Call all registered command which operation pattern match
    foreach {opmatch cmd} $Queue(waiting) {
	if { [regexp $opmatch $op] && $cmd != "" } {
	    if { [catch {eval $cmd $host $port $op $args} err] } {
		${log}::warn \
		    "Error when invoking waiting callback for $op: $err"
	    }
	}
    }
}


# ::synchro::ftp::__unlock_timeout -- Unlock on timeout
#
#	This procedure will unlock a waiting caller through setting
#	the variable passed as an argument in a manner that is
#	compatible with the __trigger procedure below, but reflects
#	that a timeout has occurred.
#
# Arguments:
#	varname	Name of lock variable.
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::synchro::ftp::__unlock_timeout { varname } {
    variable Ftp
    variable log

    if { [info exists $varname] } {
	upvar \#0 $varname Wait
	# Extract information about what we were waiting for and how
	# long we were set to wait for it.
	set opinfo [lindex $Wait 1]
	set tmout [lindex $opinfo 1]
	set op [lindex $opinfo 0]
	# Tell the caller through setting the lock variable.  Call the
	# operation __timeout to be sure it does not conflict with a
	# real operation name (to be on the safe side), be sure to
	# give the caller some extra information about how the timeout
	# actually occurred.
	set Wait [list "__timeout" "Timeout, waited $tmout ms for op: $op"]
    }
}


# ::synchro::ftp::__trigger -- Set a variable with proc's arguments
#
#	This procedure will ensure that the namespace variable passed
#	as an argument will be set to the operation and arguments
#	passed as arguments, reflecting as such what happened.  This
#	is used to trigger, and thus, unlock a caller that is waiting
#	on that very variable.
#
# Arguments:
#	varname	Name of lock variable.
#	host	Host name of FTP server
#	port	Port of FTP server
#	op	Operation that occurred
#	args	Additional arguments to operation
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::synchro::ftp::__trigger { varname host port op args } {
    variable Ftp
    variable log

    upvar \#0 $varname Wait
    # Tell the caller that the operation has occurred through setting
    # the lock variable with the operation and all its arguments.
    set Wait [list $op $args]
}


# ::synchro::ftp::__waitop -- Wait for operation
#
#	Synchronously wait for a given operation (or an error) to
#	occur within a given FTP connection.
#
# Arguments:
#	host	Host name of FTP server
#	port	Port of FTP server
#	op	Operation to wait for
#	args_p	Pointer to variable that will contain arguments of the op
#
# Results:
#	Return the operation that was received, i.e. either the operation
#       passed as a parameter, either "error", either "timeout"
#
# Side Effects:
#	Will block the process until timeout or operation occured.
proc ::synchro::ftp::__waitop { host port op {args_p ""} {tmout 5000}} {
    variable Ftp
    variable log

    if { $port == "" } { set port $Ftp(default_port) }
    set queue "${host}_${port}"
    set varname "::synchro::ftp::Queue_${queue}"
    upvar \#0 $varname Queue

    # Create a synchronisation variable
    set id [incr Ftp(idgene)]
    set varname "::synchro::ftp::WaitOp_${id}"
    upvar \#0 $varname Wait
    set Wait [list "__waiting" [list $op $tmout]]
    
    # Add our own callback to the list of waiting callbacks and wait
    # for the operation (or an error) to happen.
    lappend Queue(waiting) \
	"(error|$op)" [list ::synchro::ftp::__trigger $varname]
    after $tmout ::synchro::ftp::__unlock_timeout $varname
    vwait $varname

    # Remove the callback that we created above.
    set i 0
    foreach {opmatch cmd} $Queue(waiting) {
	if { [lindex $cmd 1] == $varname } {
	    set Queue(waiting) [lreplace $Queue(waiting) $i [expr {$i + 1}]]
	    break
	}
	incr i 2
    }

    # And return the result to the caller.
    if { $args_p != "" } {
	upvar $args_p args
	set args [lindex $Wait 1]
    }
    set op [lindex $Wait 0]
    if { $op == "__timeout" } { set op "timeout" }
    unset Wait; # This will remove the variable to avoid the timeout to trigger
    return $op
}


# ::synchro::ftp::__checkcompletion -- Check fetch completion
#
#	Check wether a file has been fetched completely from the FTP
#	server or not.  If had successfully asked the FTP server about
#	the size of the file, completion is decided whence local and
#	remote sizes are equal.  If not, completion is decided whence
#	local size have not changed for a "sufficiently" long while.
#	For the time being this is a constant, however, that constant
#	could be server and downloading rate dependent.  Upon success,
#	the queue for the host is checked again to see if there are
#	other downloads pending.  The poll period increases, starting
#	from a low value, so that we quicker catch when small files
#	have been downloaded.  This algorithm could also be improved
#	and made dependent on the downloading rate and the server.
#
# Arguments:
#	id	Identifier of the copy in progress
#	poll	Poll period
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::synchro::ftp::__checkcompletion { id poll } {
    variable Ftp
    variable log

    # Get to information about the copy.
    set idx [lsearch $Ftp(pending) $id]
    if { $idx < 0 } {
	${log}::warn "$id does not identify an FTP operation anymore!!"
	return
    }
    
    set varname "::synchro::ftp::Copy_${id}"
    upvar \#0 $varname Copy

    # Callback progress reports.
    foreach cb $Copy(pgess_cbs) {
	if { [catch {eval $cb $id $Copy(dstfname)} err] } {
	    ${log}::warn "Error when invoking progress callback on $id: $err"
	}
    }

    # Now check the size of the current file agains the size of the
    # remote file.
    set continue_checking 1
    set poll_grow 2
    if { [file exists $Copy(dstfname)] } {
	set sz [file size $Copy(dstfname)]
	if { $Copy(size) >= 0 } {
	    # We could know the size of the remote file when preparing
	    # copy, as soon as the size of the local file is the same,
	    # we can stop polling size changes and deliver success
	    # callbacks.
	    if { $sz == $Copy(size) } {
		set continue_checking 0
	    }
	} else {
	    # The remote server was not able to send the size of the
	    # remote file, we have to mixture more.  We decide that
	    # the size has arrived when its size has not changed for a
	    # maximum period of time (Ftp(equalstop)).
	    if { $Copy(lastsize) == $sz } {
		incr Copy(nbchecks)
		set poll_grow 1; # We want to check with the same period now
		set now [clock clicks -milliseconds]
		if { $Copy(nbchecks) == 1 } {
		    # First time that the previous size of the local
		    # file and its current size are equal, remember
		    # the current time.
		    set Copy(firstequal) $now
		} elseif { [expr {$now - $Copy(firstequal)}] \
			       >= $Ftp(equalstop)} {
		    # The previous size and the current size have been
		    # equal for a number of checks and for a "long
		    # enough" period of time, stop polling for size
		    # changes, we can deliver success callbacks.
		    set continue_checking 0
		}
	    } else {
		# Remember the current size of the local file so that
		# we will be able to compare next time we poll.
		# Reinitialise the number of times sizes have been
		# equal as well.
		set Copy(lastsize) $sz
		set Copy(nbchecks) 0
	    }
	}
    }


    if { $continue_checking } {
	# We should continue checking, increase the polling period,
	# which allows us to capture quickly the arrival of small
	# files.
	set poll [expr {$poll * $poll_grow}]
	if { $poll > $Ftp(poll_max) } {
	    set poll $Ftp(poll_max)
	}
	after $poll ::synchro::ftp::__checkcompletion $id $poll
    } else {
	# The algorithm above has decided that we were done: deliver
	# success callback, mark the copy as done and check the queue
	# for its host to see if there are more FTP operations to
	# perform.
	foreach cb $Copy(done_cbs) {
	    __invokecb $cb $Copy(srcfname) $Copy(dstfname) "OK"
	}

	array set top [::uri::split $Copy(srcfname)]
	unset Copy
	set Ftp(pending) [lreplace $Ftp(pending) $idx $idx]

	if { $top(port) == "" } { set top(port) $Ftp(default_port) }
	set queue "$top(host)_$top(port)"
	set varname "::synchro::ftp::Queue_${queue}"
	upvar \#0 $varname Queue
	set Queue(inprogress) ""
	__checkqueue $top(host) $top(port)
    }
}


# ::synchro::ftp::__checkqueue -- Check downloading queue
#
#	Check the downloading queue for a given server to see whether
#	we should schedule a new fetch operation or not.
#
# Arguments:
#	host	Host name of FTP server
#	port	Port of FTP server
#
# Results:
#	None.
#
# Side Effects:
#	Will actively open connections and fetch files to the local disk.
proc ::synchro::ftp::__checkqueue { host port } {
    variable Ftp
    variable log
    
    set queue "${host}_${port}"
    set varname "::synchro::ftp::Queue_${queue}"
    upvar \#0 $varname Queue

    if { $Queue(inprogress) == "" } {
	# There is not any download in progress, peek the next from
	# the pending queue if there is any, otherwise close the
	# current connection.
	if { [llength $Queue(pending)] > 0 } {
	    # Poke next from queue and place it in Queue(inprogress).
	    set Queue(inprogress) [lindex $Queue(pending) 0]
	    set Queue(pending) [lrange $Queue(pending) 1 end]
	    set id $Queue(inprogress)
	    
	    set varname "::synchro::ftp::Copy_${id}"
	    upvar \#0 $varname Copy
	    
	    array set top [::uri::split $Copy(srcfname)]
	    # Queues are per-host, per-port, independently of the user
	    # name and password.  Close the current connection and
	    # reopen another one if we change user and password.  This
	    # is an acceptable simplification.
	    if { $Queue(ftpd) >= 0 \
		     && ( $Queue(user) != $top(user) \
			      || $Queue(passwd) != $top(pwd) ) } {
		::ftp::Close $Queue(ftpd)
		set Queue(ftpd) -1
	    }
	    
	    # We have changed user or we had not yet open the
	    # connection to the FTP server, attempt to open the
	    # connection.
	    if { $Queue(ftpd) < 0 } {
		set Queue(user) $top(user)
		set Queue(passwd) $top(pwd)
		set Queue(ftpd) \
		    [::ftp::Open $host $top(user) $top(pwd) -port $port \
			 -command [list ::synchro::ftp::__opcb $host $port]]
		set failed ""
		if { $Queue(ftpd) < 0 } {
		    set failed "Could not open FTP connection to $host:$port"
		} else {
		    if { [__waitop $host $port connect failed] == "connect" } {
			set failed ""
			::ftp::Type $Queue(ftpd) binary
			if { [__waitop $host $port type failed] == "type" } {
			    set failed ""
			}
		    }
		}

		# Opening or initialisation failed, removed the file
		# from the pending queue and try with the next
		# awaiting one.
		if { $failed != "" } {
		    ${log}::warn "Error when initialising FTP connection"
		    if { $Queue(ftpd) >= 0 } {
			::ftp::Close $Queue(ftpd)
		    }
		    foreach cb $Copy(done_cbs) {
			__invokecb $cb $Copy(srcfname) $Copy(dstfname) \
			    "ERROR" $failed
		    }
		    set Queue(inprogress) ""
		    set idx [lsearch $Ftp(pending) $id]
		    if { $idx >= 0 } {
			set Ftp(pending) [lreplace $Ftp(pending) $idx $idx]
		    }
		    after $Ftp(onerr_retry) \
			::synchro::ftp::__checkqueue $host $port
		}
	    }
	    
	    # We have an opened connection, request the remote file
	    # and wait for completion.
	    if { $Queue(ftpd) >= 0 } {
		# Request and wait for the size of the remote file
		::ftp::FileSize $Queue(ftpd) $top(path)
		if { [__waitop $host $port "size" szinfo] == "size" } {
		    set Copy(size) [lindex $szinfo end]
		} else {
		    ${log}::warn "Could not get file size of $top(path)"
		    set Copy(lastsize) 0
		    set Copy(nbchecks) 0
		    set Copy(size) -1
		}

		# Now request for the file itself, and wait to know
		# that get operation is in progress.  Ideally, we
		# would like "get" to transit back to us once the
		# fetch operation has ended, but that does not seem to
		# be working, so we poll for the result size of the
		# local file until done (see __checkcompletion)
		::ftp::Get $Queue(ftpd) $top(path) $Copy(dstfname)
		if { [__waitop $host $port "get" finfo] == "get" } {
		    after 10 ::synchro::ftp::__checkcompletion $id 10
		} else {
		    foreach cb $Copy(done_cbs) {
			__invokecb $cb $Copy(srcfname) $Copy(dstfname) \
			    "ERROR" \
			    "Error getting $Copy(srcfname), connection lost?"
		    }
		    set Queue(inprogress) ""
		    set idx [lsearch $Ftp(pending) $id]
		    if { $idx >= 0 } {
			set Ftp(pending) [lreplace $Ftp(pending) $idx $idx]
		    }
		    after $Ftp(onerr_retry) \
			::synchro::ftp::__checkqueue $host $port
		}
	    }
	} elseif { $Queue(ftpd) > 0 } {
	    ::ftp::Close $Queue(ftpd)
	    set Queue(ftpd) -1
	}
    }
}


# ::synchro::file::cancel -- Cancel an FTP copy
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
proc ::synchro::ftp::cancel { id } {
    variable Ftp
    variable log

    set gidx [lsearch $Ftp(pending) $id]
    if { $gidx < 0 } {
	${log}::warn "$id does not identify a file copy anymore!!"
	return
    }
    
    set varname "::synchro::file::Copy_${id}"
    upvar \#0 $varname Copy

    array set top [uri::split $dirtop]
    if { $top(port) == "" } { set top(port) $Ftp(default_port) }
    set queue "$top(host)_$top(port)"
    set varname "::synchro::ftp::Queue_${queue}"
    upvar \#0 $varname Queue

    foreach cb $Copy(done_cbs) {
	__invokecb $cb $Copy(srcfname) $Copy(dstfname) "CANCEL"
    }
    if { $Queue(inprogress) == $id } {
	# Only one choice: close the connection!
	catch {::ftp::Close $Queue(ftpd)}
	set Queue(ftpd) -1
	set Queue(inprogress) ""
    } else {
	set idx [lsearch $Queue(pending) $id]
	if { $idx >= 0 } {
	    set Queue(pending) [lreplace $Queue(pending) $idx $idx]
	} else {
	    ${log}::warn "$id is not in any FTP awaiting queue (yet?)"
	}
    }
    set Ftp(pending) [lreplace Ftp(pending) $gidx $gidx]
    after idle ::synchro::ftp::__checkqueue $top(host) $top(port)
}


# ::synchro::ftp::geturl -- Copy a file
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
#	once!).
#
# Arguments:
#	dirtop	Top directory under which the source file is.
#	src	Relative path to the source file.
#	dst_fp	Name of destination file.
#	donecb	Command to call back at completion.
#	bytes	How many bytes to copy from file?
#
# Results:
#	An identifier of the copy operation or empty string on error.
#
# Side Effects:
#	Will copy file on the local hard drive!
proc ::synchro::ftp::geturl { dirtop src dst_fp donecb {pgesscb ""} {bytes -1}} {
    variable Ftp
    variable log

    if { $bytes >= 0 } {
	return -code error "Partial downloading not yet implemented!"
    }

    set src_fp $dirtop
    if { [string index $src_fp end] != "/" } { append src_fp "/" }
    append src_fp $src

    # Append the callback to any existing pending copy
    foreach id $Ftp(pending) {
	set varname "::synchro::ftp::Copy_${id}"
	upvar \#0 $varname Copy

	if { $Copy(srcfname) == $src_fp && $Copy(dstfname) == $dst_fp } {
	    lappend Copy(done_cbs) $donecb
	    if { $pgesscb != "" } {
		lappend Copy(pgess_cbs) $pgesscb
	    }
	    return $Copy(id)
	}
    }

    array set top [uri::split $dirtop]
    if { $top(port) == "" } { set top(port) $Ftp(default_port) }

    # Otherwise, store copying information, place in queue for host
    # and check the queue.
    set id [incr Ftp(idgene)]
    set varname "::synchro::ftp::Copy_${id}"
    upvar \#0 $varname Copy

    set Copy(id) $id
    set Copy(srcfname) $src_fp
    set Copy(dstfname) $dst_fp
    set Copy(dstfdes) ""
    set Copy(done_cbs) [list $donecb]
    if { $pgesscb == "" } {
	set Copy(pgess_cbs) {}
    } else {
	set Copy(pgess_cbs) [list $pgesscb]
    }
    lappend Ftp(pending) $id
    
    set queue "$top(host)_$top(port)"
    set varname "::synchro::ftp::Queue_${queue}"
    upvar \#0 $varname Queue
    set idx [lsearch $Ftp(queues) $queue]
    if { $idx < 0 } {
	set Queue(host) $top(host)
	set Queue(port) $top(port)
	set Queue(pending) [list $id]
	set Queue(user) ""
	set Queue(passwd) ""
	set Queue(ftpd) -1
	set Queue(inprogress) ""
	lappend Ftp(queues) $queue
    } else {
	lappend Queue(pending) $id
    }

    __checkqueue $top(host) $top(port)

    return $id
}
