# crontab.tcl --
#
#	This modules provides a set of routines to parse and execute
#       crontab-like files.
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

package require Tcl 8.2
package require logger
package require cron
package require diskutil

package provide crontab 1.0

namespace eval ::crontab {
    # Initialise the global state
    variable CRONTAB
    if {![::info exists CRONTAB]} {
	array set CRONTAB {
	    loglevel  warn
	}
	variable log [::logger::init crontab]
	${log}::setlevel $CRONTAB(loglevel)
    }

    namespace export read addline
}

# ::crontab::loglevel -- Set/Get current log level.
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
proc ::crontab::loglevel { { loglvl "" } } {
    variable CRONTAB
    variable log

    if { $loglvl != "" } {
	if { [catch "${log}::setlevel $loglvl"] == 0 } {
	    set CRONTAB(loglevel) $loglvl
	}
    }

    return $CRONTAB(loglevel)
}


# ::crontab::__proccall --
#
#	Call a declared accessible procedure with arguments.  This
#	command complies to the cron library interface.
#
# Arguments:
#	when	Time at which the procedure is executed.
#	pname	Name of procedure to be called
#	args	Arguments to command
#
# Results:
#	None.
#
# Side Effects:
#	Whichever side effect the called procedure has.
proc ::crontab::__proccall { when pname args } {
    variable log

    ${log}::info "Calling procedure $pname $args"
    catch "eval $pname $args" result
    ${log}::info "Procedure $pname returned $result"
}


# ::crontab::__exec --
#
#	Execute an external command with arguments.  This command
#	complies to the cron library interface.
#
# Arguments:
#	when	Time at which the procedure is executed.
#	cmd	Command to be executed
#	args	Arguments to command
#
# Results:
#	None.
#
# Side Effects:
#	Whichever side effect the called procedure has.
proc ::crontab::__exec { when cmd args } {
    variable log

    ${log}::info "Calling external command $cmd $args"
    catch "eval exec [auto_execok $cmd] $args" result
    ${log}::info "Command $cmd returned $result"
}


# ::crontab::__spec_analyse --
#
#	Analyse a crontab specification line.
#
# Arguments:
#	line	Line to be analysed
#	keyword	Keyword that should be leading the command (PROC, EXEC)
#
# Results:
#	The analysis returns a list with 6 or seven arguments (or an
#	empty one in case of failure).  The first 5 arguments are the
#	scheduling specification, the last ones the command and its
#	arguments.
#
# Side Effects:
#	None.
proc ::crontab::__spec_analyse { line keyword } {
    variable log

    set upper_line [string toupper $line]
    set key_idx [string first $keyword $upper_line]
    if { $key_idx >= 0 } {
	set spec_list \
		[string trim [string range $line 0 $key_idx]]
	if { [llength $spec_list] < 5 } {
	    ${log}::warn "$line is not a valid cron spec"
	} else {
	    set remaining \
		    [string trim \
		    [string range $line \
		    [expr $key_idx + [string length $keyword]] end]]
	    set space [string first " " $remaining]
	    if { $space < 0 } {
		return [list \
			[lindex $spec_list 0] \
			[lindex $spec_list 1] \
			[lindex $spec_list 2] \
			[lindex $spec_list 3] \
			[lindex $spec_list 4] \
			$remaining]
	    } else {
		set procname \
			[string range $remaining \
			0 [expr $space - 1]]
		return [list \
			[lindex $spec_list 0] \
			[lindex $spec_list 1] \
			[lindex $spec_list 2] \
			[lindex $spec_list 3] \
			[lindex $spec_list 4] \
			$procname \
			[string range $remaining \
			[expr $space + 1] end]]
	    }
	}
    }

    return ""
}


# ::crontab::addline --
#
#	Add the specification contained within one string into the
#	current cron context
#
# Arguments:
#	line	Line containing the whole specification
#
# Results:
#	Returns the cron identifier for the entry or an empty string
#	on error.
#
# Side Effects:
#	None.
proc ::crontab::addline { line } {
    set l_proc [::crontab::__spec_analyse $line "PROC"]
    if { [llength $l_proc] > 0 } {
	set cronid [::cron::add \
			[lindex $l_proc 0] \
			[lindex $l_proc 1] \
			[lindex $l_proc 2] \
			[lindex $l_proc 3] \
			[lindex $l_proc 4] \
			::crontab::__proccall \
			[lindex $l_proc 5] \
			[lindex $l_proc 6]]
	return $cronid
    }
    
    set l_exec [::crontab::__spec_analyse $line "EXEC"]
    if { [llength $l_exec] > 0 } {
	set cronid [::cron::add \
			[lindex $l_exec 0] \
			[lindex $l_exec 1] \
			[lindex $l_exec 2] \
			[lindex $l_exec 3] \
			[lindex $l_exec 4] \
			::crontab::__exec \
			[lindex $l_exec 5] \
			[lindex $l_exec 6]]
	return $cronid
    }

    return ""
}


# ::crontab::read --
#
#	This command reads a crontab-like file and registers
#	corresponding entries within the cron module.
#
# Arguments:
#	fname	Name of file to read
#
# Results:
#	Returns the number of entries registered or a negative number
#	on error.
#
# Side Effects:
#	None.
proc ::crontab::read { fname } {
    variable CRONTAB
    variable log

    ${log}::info "Reading crontab $fname"

    set nbentries 0
    if { [catch "open $fname" fd] == 0 } {
	::cron::bursting 1

	while { ![eof $fd] } {
	    set line [string trim [gets $fd]]
	    if { $line != "" } {
		set firstchar [string index $line 0]
		if { $firstchar != "\#" && $firstchar != ";" } {
		    if { [string match -nocase "INCLUDE*" $line] } {
			set files [string trim \
				       [string range $line \
					    [string length "INCLUDE"] end]]
			set files [::diskutil::fname_resolv $files]
			foreach f_include $files {
			    uplevel \#0 "source $f_include"
			}
		    } else {
			set res [::crontab::addline $line]
			if { $res != "" } {
			    incr nbentries
			}
		    }
		}
	    }
	}

	close $fd
	::cron::bursting 0
    } else {
	${log}::warn "Could not find crontab file $fname"
	return -1
    }

    return $nbentries
}

