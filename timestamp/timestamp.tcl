# timestamp.tcl --
#
#	Timestamping facilities, both acquisition and parsing from string.
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

package require Tcl 8.2

package provide timestamp 1.0

namespace eval ::timestamp {
    namespace export get scan
}


# ::timestamp::get --
#
#	This procedure implements timestamping accurate to the
#	millisecond (as much as the OS and Tcl can do).
#
# Arguments:
#	tv_sec_p	"pointer" to variable that will contain current seconds
#	tv_msec_p	"pointer" to variable that will contain current msecs
#
# Results:
#	Returns a string formated as secs.msecs
#
# Side Effects:
#	None.
proc ::timestamp::get { { tv_sec_p "" } { tv_msec_p "" } } {
    if { $tv_sec_p != "" } {
	upvar $tv_sec_p secs
    }
    if { $tv_msec_p != "" } {
	upvar $tv_msec_p fract
    }

    set secs [clock seconds]
    set ms [clock clicks -milliseconds]
    set base [expr { $secs * 1000 }]
    set fract [expr { $ms - $base }]
    if { $fract > 1000 } {
	set diff [expr { $fract / 1000 }]
	incr secs $diff
	incr fract [expr { -1000 * $diff }]
    }

    return $secs.[format %03d $fract]
}


# ::timestamp::scan --
#
#	Analyse a string and attempts to analyse the time stamp that
#	it contains. Recognised formats are: secs.msecs, secs (where
#	secs are the number of seconds since the POSIX period) and any
#	other date accepted by the Tcl clock scan command.
#
# Arguments:
#	time	Time string
#	sec_p	"Pointer" to seconds container
#	msec_p	"Pointer" to milliseconds container
#
# Results:
#	Return the time formatted as secs.msecs, an empty string if
#	analysis was impossible.
#
# Side Effects:
#	None.
proc ::timestamp::scan { time { sec_p "" } { msec_p "" } } {
    if { $sec_p != "" } {
	upvar $sec_p now_sec
    }

    if { $msec_p != "" } {
	upvar $msec_p now_msec
    }

    if { [regexp -- "-?\\d+\\.\\d+" $time] } {
	set dot [string first "." $time]
	set now_sec [string range $time 0 [expr $dot - 1]]
	set now_msec [string trimleft \
			  [string range $time [expr $dot + 1] end] 0]
	if { $now_msec == "" } {
	    set now_msec 0
	}
    } elseif { [regexp -- "-?\\d+" $time] } {
	set now_sec $time
	set now_msec 0
    } else {
	if { [catch "clock scan $time" now_sec] != 0 } {
	    return ""
	}
	set now_msec 0
    }
    set now $now_sec.[format %03d $now_msec]

    return $now
}
