# preprocessor.tcl --
#
#	This module implements a simplistic XML parser in 10 lines of
#	code.  It is mostly suitable for small files, but has
#	successfully been tested on larger files.  The code is based
#	heavily on Stephen Uhler's HTML parser in 10 lines, modified
#	by Eric Kemp-Benedict for XML (see http://wiki.tcl.tk/14534)
#	and revisited to offer a number of additional features for the
#	TIL.
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

# We should offer the possibility to check for condition while
# ignoring upper/lower cases, and to execute only one or all rules.
# Declaring the preprocessor object with -started options is a good
# idea.  Recognising such options in the rules as well.

package require Tcl 8.4
package require logger

package provide preprocessor 0.1

namespace eval ::preprocessor {
    # Initialise the global state
    variable PP
    if {![::info exists PP]} {
	array set PP {
	    idgene        0
	    loglevel      "warn"
	    processors    ""
	}
	variable log [::logger::init [string trimleft [namespace current] ::]]
	${log}::setlevel $PP(loglevel)
    }
    namespace export new process
}


# ::preprocessor::loglevel -- Set/Get current log level.
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
proc ::preprocessor::loglevel { { loglvl "" } } {
    variable PP
    variable log

    if { $loglvl != "" } {
	if { [catch "${log}::setlevel $loglvl"] == 0 } {
	    set PP(loglevel) $loglvl
	}
    }

    return $PP(loglevel)
}



proc ::preprocessor::process { id line } {
    variable PP
    variable log

    set idx [lsearch $PP(processors) $id]
    if { $idx < 0 } {
	${log}::warn "Preprocessor $id does not exist!"
	return $line
    }

    set varname "::preprocessor::cx_${id}"
    upvar \#0 $varname PROCESSOR
    foreach r $PROCESSOR(rules) {
	foreach { type trigger inexp replace } $r {}
	set docmd 0
	set utype [string toupper $type]
	switch -- $utype {
	    RX -
	    RXCB -
	    REGEXPCB -
	    REGEXP {
		if { [catch {regexp $trigger $line} res] } {
		    ${log}::warn \
			"Could not execute regular exp $trigger: $res!"
		} else {
		    set docmd $res
		    if { $docmd } {
			${log}::debug "$line matches regexp $trigger"
		    }
		}
	    }
	    SM -
	    SMCB -
	    STRINGMATCHCB -
	    STRINGMATCH {
		set docmd [string match $trigger $line]
		if { $docmd } {
		    ${log}::debug "$line matches string pattern $trigger"
		}
	    }
	}

	if { $docmd } {
	    if { [lsearch [list RX REGEXP SM STRINGMATCH] $utype] >= 0 } {
		if { [catch {regsub -all $inexp $line $replace} res] } {
		    ${log}::warn "Cannot execute regular exp $inexp: $res!"
		} else {
		    set line $res
		}
	    }
	    if { [lsearch [list RXCB REGEXPCB SMCB STRINGMATCHCB] \
		      $utype] >= 0 } {
		if { [catch {eval $inexp \$line} res] } {
		    ${log}::warn "Cannot execute user callback $inexp: $res!"
		} else {
		    set line $res
		}
	    }
	}
    }

    if { $PROCESSOR(cb) ne "" } {
	if { [catch {eval $PROCESSOR(cb) $id \$line} res] } {
	    ${log}::warn "Error when invoking cb $PROCESSOR(cb): $res"
	}
    }

    return $line
}


# ::preprocessor::__pusher -- Push read lines from file for processing
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
proc ::preprocessor::__pusher { id fd } {
    variable PP
    variable log

    while { ! [eof $fd] } {
	set line [gets $fd]
	::preprocessor::process $id $line
    }
    close $fd
}


# ::preprocessor::new -- Process a file 
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
proc ::preprocessor::new { { rules {} } { linecb "" } { fname "" } } {
    variable PP
    variable log

    if { $fname ne "" } {
	if { [catch {open $fname} fd] } {
	    ${log}::warn "Could not open initial file '$fname': $fd. Aborting!"
	    return ""
	}
    }

    set id [incr PP(idgene)]
    set varname "::preprocessor::cx_${id}"
    upvar \#0 $varname PROCESSOR

    set PROCESSOR(id) $id
    set PROCESSOR(rules) $rules
    set PROCESSOR(cb) $linecb
    lappend PP(processors) $id

    if { $fname ne "" } {
	after idle ::preprocessor::__pusher $id $fd
    }

    return $id
}

