# cron.tcl --
#
#	This modules provides a set of routines to provide a cron-like
#	facility.
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

package require Tcl 8.2
package require logger

package provide cron 1.0

namespace eval ::cron {
    # Variables of name cron::__entry_<id> are created as arrays to
    # support each cron directive.

    # Initialise the global state
    variable CRON
    if {![::info exists CRON]} {
	array set CRON {
	    idgene    0
	    entries   ""
	    bursting  0
	    loglevel  warn
	}
	variable log [::logger::init cron]
	${log}::setlevel $CRON(loglevel)
    }

    namespace export bursting add info delete checkonce loglevel
}


# ::cron::loglevel -- Set/Get current log level.
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
proc ::cron::loglevel { { loglvl "" } } {
    variable CRON
    variable log

    if { $loglvl != "" } {
	if { [catch "${log}::setlevel $loglvl"] == 0 } {
	    set CRON(loglevel) $loglvl
	}
    }

    return $CRON(loglevel)
}



# ::cron::__fieldmatch --
#
#	This command matches a crontab-like specification for a field
#	to a current value.
#
#	A field may be an asterisk (*), which always stands for
#	''first-last''.
#
#	Ranges of numbers are allowed.  Ranges are two numbers
#	separated with a hyphen.  The specified range is inclusive.
#	For example, 8-11 for an ''hours'' entry specifies execution
#	at hours 8, 9, 10 and 11.
#
#	Lists are allowed.  A list is a set of numbers (or ranges)
#	separated by commas.  Examples: ''1,2,5,9'', ''0-4,8-12''.
#
#	Step values can be used in conjunction with ranges.  Following
#	a range with ''/<number>'' specifies skips of the number's
#	value through the range.  For example, ''0-23/2'' can be used
#	in the hours field to specify command execution every other
#	hour (the alternative in the V7 standard is
#	''0,2,4,6,8,10,12,14,16,18,20,22'').  Steps are also permitted
#	after an asterisk, so if you want to say ''every two hours'',
#	just use ''*/2''.
#
# Arguments:
#	value	Current value of the field
#	spec	Matching specification
#
# Results:
#	returns 1 if the current value matches the specification, 0
#	otherwise
#
# Side Effects:
#	None.
proc ::cron::__fieldmatch { value spec } {
    if { $value != "0" } {
	regsub "^0" $value "" value
    }

    foreach rangeorval [split $spec ","] {

	# Analyse step specification
	set idx [string first "/" $rangeorval]
	if { $idx >= 0 } {
	    set step [string trim \
			  [string range $rangeorval [expr $idx + 1] end]]
	    set rangeorval [string trim \
				[string range $rangeorval 0 [expr $idx - 1]]]
	} else {
	    set step 1
	    set rangeorval [string trim $rangeorval]
	}

	# Analyse range specification.
	set values ""
	set idx [string first "-" $rangeorval]
	if { $idx >= 0 } {
	    set minval [string trim \
			    [string range $rangeorval 0 [expr $idx - 1]]]
	    if { $minval != "0" } {
		regsub "^0" $minval "" minval
	    }
	    set maxval [string trim \
			    [string range $rangeorval [expr $idx + 1] end]]
	    if { $maxval != "0" } {
		regsub "^0" $maxval "" maxval
	    }
	    for { set i $minval } { $i <= $maxval } { incr i $step } {
		if { $value == $i } {
		    return 1
		}
	    }
	} else {
	    if { $rangeorval == "*" } {
		if { ! [expr int(fmod($value, $step))] } {
		    return 1
		}
	    } else {
		if { $rangeorval == $value } {
		    return 1
		}
	    }
	}
    }

    return 0
}


# ::cron::__check --
#
#	Check whether it it time to trigger an entry and trigger the
#	associated callback if it is.
#
# Arguments:
#	id	Identifier of entry
#	nxcheck	When to perform the next check
#
# Results:
#	Returns 1 if the entry was triggered, 0 if not and a negative
#	on error.
#
# Side Effects:
#	This command reschedule a check to be performed in nxcheck
#	milliseconds if non negative and not an empty string
proc ::cron::__check { id { nxcheck "" } { now "" } } {
    variable CRON
    variable log

    set idx [lsearch $CRON(entries) $id]
    if { $idx >= 0 } {
	set varname "::cron::__entry_$id"
	upvar \#0 $varname Entry

	if { $now eq "" } {
	    set now [clock seconds]
	}
	set min [clock format $now -format "%M"]
	set hour [clock format $now -format "%H"]
	set daymonth [clock format $now -format "%e"]
	set month [clock format $now -format "%m"]
	set dayweek [clock format $now -format "%w"]
	set triggered 0
	if { [::cron::__fieldmatch $min $Entry(min)] \
		 && [::cron::__fieldmatch $hour $Entry(hour)] \
		 && [::cron::__fieldmatch $daymonth $Entry(daymonth)] \
		 && [::cron::__fieldmatch $month $Entry(month)] \
		 && [::cron::__fieldmatch $dayweek $Entry(dayweek)] } {
	    ${log}::info "Triggering entry $id - $Entry(cmd)"
	    uplevel \#0 [linsert $Entry(args) 0 $Entry(cmd) $now]
	    set triggered 1
	}

	if { $nxcheck != "" && $nxcheck >= 0 } {
	    set Entry(checkid) [after $nxcheck ::cron::__check $id $nxcheck]
	}

	return $triggered
    }
    return -1
}



# ::cront::bursting --
#
#	Set/Get bursting mode.  In bursting mode, several entries are
#	awaited to be created rather close to one another and they are
#	evenly spread out along the first cron minute check.
#
# Arguments:
#	burst	New burst mode (1, 0 or empty simply to query)
#
# Results:
#	Return the current bursting mode.
#
# Side Effects:
#	None.
proc ::cron::bursting { { burst "" } } {
    variable CRON

    if { $burst != "" } {
	set CRON(bursting) $burst
    }
    return $CRON(bursting)
}



# ::cron::checkonce -- Force one time trigger check
#
#	This command will force one or more entries to be checked
#	given a date passed as argument and will trigger these if
#	appropriate.
#
# Arguments:
#	date	Date specification (clock scan!)
#	entries	List of entries to check (empty for all known)
#
# Results:
#	A list with the result of the triggering: for each entry, 0 if
#	not triggered, 1 if triggered, negative if error (probably not
#	a known entry).
#
# Side Effects:
#	None.
proc ::cron::checkonce { date { entries "" } } {
    variable CRON
    variable log

    if { $entries eq "" } {
	set entries $CRON(entries)
    }

    set res {}
    
    if { [catch {clock scan $date} now] } {
	${log}::error "Could not understand $date as a date, aborting: $now"
	foreach id $entries {
	    lappend res -1
	}
    } else {
	foreach id $entries {
	    lappend res [__check $id "" $now]
	}
    }

    return $res
}



# ::cron::add --
#
#	Add a new command to be called back regularly.  When the time
#	of the day matches the specification, the command passed as an
#	argument will be called back with the time as as a first
#	argument, followed by the arguments passed to "add" as an
#	argument.
#
# Arguments:
#	min	Minute specification
#	hour	Hour specification
#	daymonth	Day of month specification
#	month	Month specification
#	dayweek	Day of week specification
#	cmdproc	Command to be called back
#	args	Arguments to pass back to call back each time.
#
# Results:
#	An identifier to be used in all other function calls of this
#	library.
#
# Side Effects:
#	None.
proc ::cron::add { min hour daymonth month dayweek cmdproc args } {
    variable CRON
    variable log

    set id [incr CRON(idgene)]
    set varname "::cron::__entry_$id"
    upvar \#0 $varname Entry

    set Entry(min) $min
    set Entry(hour) $hour
    set Entry(daymonth) $daymonth
    set Entry(month) $month
    set Entry(dayweek) $dayweek
    set Entry(cmd) $cmdproc
    set Entry(args) $args
    set Entry(id) $id
    if { $CRON(bursting) } {
	set when [expr int(rand()*60000)]
	set Entry(checkid) [after $when ::cron::__check $id 60000]
	${log}::notice "Scheduled new entry \#$id to be checked in $when ms."
    } else {
	set Entry(checkid) [after idle ::cron::__check $id 60000]
	${log}::notice "Scheduled new entry \#$id to be checked asap."
    }
    
    lappend CRON(entries) $id

    return $id
}



# ::cron::info --
#
#	Return information on one or several cron entries.  Returns
#	information for each entry passed as an argument.  An empty
#	argument list means all entries.
#
# Arguments:
#	args	(list) of entries to get information for
#
# Results:
#	Return a list of lists, one for each entry which id was passed
#	in the arguments.  For each entry, there will be one
#	descripting list of 8 arguments: the identifier of the entry,
#	the 5 scheduling specifications, the procedure and its
#	arguments.
#
# Side Effects:
#	None.
proc ::cron::info { args } {
    variable CRON
    variable log

    if { [llength $args] <= 0 } {
	set query $CRON(entries)
    } else {
	set query $args
    }

    set res ""
    foreach id $query {
	set idx [lsearch $CRON(entries) $id]
	if { $idx >= 0 } {
	    set varname "::cron::__entry_$id"
	    upvar \#0 $varname Entry

	    lappend res [list $id \
		    $Entry(min) \
		    $Entry(hour) \
		    $Entry(daymonth) \
		    $Entry(month) \
		    $Entry(dayweek) \
		    $Entry(cmd) \
		    $Entry(args)]
	} else {
	    ${log}::warn "$id is not the identifier of a known entry"
	    lappend res [list $id]
	}
    }

    return $res
}



# ::cron::delete --
#
#	Delete an existing command.
#
# Arguments:
#	id	Identifier of cron entry, as returned by ::cron::add
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::cron::delete { id } {
    variable CRON
    variable log

    set idx [lsearch $CRON(entries) $id]
    if { $idx >= 0 } {
	set varname "::cron::__entry_$id"
	upvar \#0 $varname Entry

	${log}::notice "Removing entry $id - $Entry(cmd)"
	after cancel $Entry(checkid)
	unset Entry
	set CRON(entries) [lreplace $CRON(entries) $idx $idx]
    } else {
	${log}::warn "$id is not the identifier of a known entry"
    }
}

