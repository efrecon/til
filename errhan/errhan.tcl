# errhan.tcl --
#
#	Provides a simple error handling module that allows
#	application to possibly continue after an error.
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.


package require Tcl 8.2

package provide errhan 1.1

namespace eval ::errhan {
    variable EH
    if {![info exists EH]} {
	array set EH {
	    loglevel   warn
	    err_cb     {}
	    latest     {}
	    limit_err  1
	    limit_time 4000
	}
	variable log [::logger::init [string trimleft [namespace current] ::]]
	${log}::setlevel $EH(loglevel)
    }
    
    namespace export add loglevel
}


# ::errhan::loglevel -- Set/Get current log level.
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
proc ::errhan::loglevel { { loglvl "" } } {
    variable EH
    variable log

    if { $loglvl != "" } {
	if { [catch "${log}::setlevel $loglvl"] == 0 } {
	    set EH(loglevel) $loglvl
	}
    }

    return $EH(loglevel)
}


# bgerror --
#
#	Reroute background errors to the implementation of this
#	module.  This code is partially inherited from tkcon.
#
#
# Arguments:
#	err     	Error description
#
# Results:
#	None.
#
# Side Effects:
#	None directly, see ::errhan::bgerror
proc ::bgerror { err } {
    global errorInfo

    set body [info body bgerror]
    rename ::bgerror {}
    if { 0 && [auto_load bgerror]} {
	rename ::bgerror ::__autoloaded_bgerror
	proc ::bgerror err $body
	if { [::errhan::bgerror $err $errorInfo 1] } {
	    set res [::__autoloaded_bgerror $err]
	    rename ::__autoloaded_bgerror {}
	    return $res
	}
    }
    proc ::bgerror err $body
    ::errhan::bgerror $err $errorInfo
}



# ::errhan::bgerror --
#
#	Handle background errors and give a chance to other modules to
#	decide whether this is an error that should lead to an exit or
#	not. The callbacks that have registered interest in background
#	errors should return a boolean.  If that boolean is true, then
#	the error is all-right and execution should continue.  If not,
#	then execution should stop, unless another callback has
#	decided otherwise.
#
#
# Arguments:
#	err     	Error description
#	errInfo     	Error information message
#	continue     	Continue to caller on fatal error (no auto-exit)
#
# Results:
#	Return if the error was fatal and the process should exit or not.
#
# Side Effects:
#	If one of the callbacks has specified that the error should
#	proceed or if no callback is registered, the error is printed
#	out on stderr and the process will exit.
proc ::errhan::bgerror { err errorInfo {continue 0}} {
    variable EH
    variable log

    ${log}::warn "Error '$err', forwarding it further"
    set do_exit 1
    if { [llength $EH(err_cb)] > 0 } {
	set error_ok 0
	foreach cb $EH(err_cb) {
	    ${log}::debug "Forwarding error '$err' to callback: $cb"
	    if { [$cb $err] } {
		${log}::info "Error '$err' catched and acknowledged"
		set error_ok 1
		break
	    }
	}
	if { $error_ok } {
	    set do_exit 0
	}
    }

    # Add current error and time to list of latest errors.
    set now [clock clicks -milliseconds]
    lappend EH(latest) $now $err $errorInfo

    # Arrange to keep only the errors that are within the time limit.
    set latest [list]
    set size 0
    foreach {when e e_info} $EH(latest) {
	if { $when >= [expr {$now - $EH(limit_time)}] } {
	    lappend latest $when $e $e_info
	    incr size
	}
    }
    set EH(latest) $latest

    if { !$continue && $do_exit } {
	puts stderr "ERROR in Tcl Script: $err"
	puts stderr "$errorInfo"

	if { $size >= $EH(limit_err) } {
	    exit
	} else {
	    ${log}::error "$size (out of $EH(limit_err) allowed) errors within\
                           the last $EH(limit_time) ms., continuing..."
	}
    }

    return $do_exit
}


# ::errhan::add --
#
#	Add a procedure to be called on background errors.
#
#
# Arguments:
#	proc_cb     	Procedure to be called back
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::errhan::add { proc_cb } {
    variable EH
    variable log

    ${log}::info "Adding new error handler: $proc_cb"
    lappend EH(err_cb) $proc_cb
}


proc ::errhan::exit_condition { { limit_err -1 } { limit_time -1 } } {
    variable EH
    variable log

    set old [list $EH(limit_err) $EH(limit_time)]
    if { $limit_err >= 0 } {
	set EH(limit_err) $limit_err
    }
    if { $limit_time >= 0 } {
	set EH(limit_time) $limit_time
    }
    ${log}::info "Allowing a maximum of $EH(limit_err) error(s) per\
                  $EH(limit_time) ms. before exiting."

    return $old
}
