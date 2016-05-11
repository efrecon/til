#!/bin/sh
# the next line restarts using tclsh \
exec tclsh "$0" "$@"

##################
## Program Name    --  timestamper.tcl
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##   This program implements timestamps everything coming from the
##   standard input before printing it to a file.
##
##################
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.


# Array Name       --  TS
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Global state of this program
#
# Contents:
#    finish	- End indicator
#    outfile	- File to write to
array set TS {
    finish         0
    infile         ""
    outfile        ""
    outid          ""
}

source [file join [file dirname $argv0] argutil.tcl]
argutil::accesslib tcllib

# Now parse the options and put the result into the TS global state array
package require cmdline

set options {
    { verbose.arg "critical" "Verbosity Level" }
    { datesensitive "Should we skip leading date output if it exists?" }
    { ignorepreformat "Should we skip timestamping of preformatted lines?" }
    { endoneof "Should we die ourselves when eof has been detected?" }
    { rotate.double "168" "Number of hours before rotating, negative to switch off" }
    { keep.integer "6" "How many log files should we keep when rotating?" }
    { maxsize.integer "134217728" "Maximum size of output files" }
    { outfile.arg "stdout" "Output file, empty means stdout" }
    { infile.arg "stdin" "Input file" }
    { triggers.args "@%progdir%/%progname%.trg" "Specifications of triggers" }
    { dateformat.arg "\[%d%m%y %H:%M:%S\] " "Date format to prepend to lines" }
}

set inited [argutil::initargs TS $options]
if { [catch {cmdline::typedGetoptions argv $options} optlist] != 0 } {
    puts [cmdline::typedUsage $options "accepts the following options:"]
    exit
}
array set TS $optlist
argutil::boolean TS ignorepreformat
argutil::boolean TS datesensitive
argutil::boolean TS endoneof
foreach key $inited {
    argutil::makelist TS($key)
}

# Include modules that we depend on.  This is complicated to be able
# to address separately modules in the verbose specification.
argutil::accesslib til
argutil::accesslib eflib
argutil::loadmodules [list outlog diskutil logwatch uobj] $TS(verbose)
set TS(log) [::logger::init [file rootname [file tail [info script]]]]
$TS(log)::setlevel $TS(verbose)


# Command Name     --  in_between
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Extracts sub strings between separators and return content from there.
#
# Arguments:
#    str	- Incoming string
#    open_sep	- Opening separator
#    close_sep	- Closing separator (same as open_sep if missing or empty)
#    searchstart- Index of character at which to start the search
proc in_between { str open_sep { close_sep "" } { searchstart 0 } } {
    set substr ""
    if { $close_sep == "" } {
	set close_sep $open_sep
    }

    set start [string first $open_sep $str $searchstart]
    if { $start >= 0 } {
	incr start [string length $open_sep]

	set end [string first $close_sep $str $start]
	if { $end >= 0 } {
	    incr end -1
	    set substr [string range $str $start $end]
	}
    }

    return $substr
}


# Command Name     --  log_out
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Command called back when log lines have been detected.
#
# Arguments:
#    wid	- Identifier of watch
#    line	- Line of log
proc log_out { wid line } {
    global TS

    set dump 1
    if { $TS(ignorepreformat) } {
	if { $line == "" || [string index $line 0] == "\t" \
		 || [string index $line 0] == " " } {
	    set dump 0
	}
    }

    set dumpdate 1
    if { $TS(datesensitive) } {
	set dt [in_between $line "\[" "\]"]
	if { [catch {clock scan $dt} now] == 0 } {
	    set dumpdate 0
	} else {
	    set now ""
	}
    }

    if { $dump } {
	if { $dumpdate && $TS(dateformat) != "" } {
	    set now [clock seconds]
	    set dt "[clock format $now -format $TS(dateformat)]"
	    ::outlog::puts $TS(outid) "${dt}${line}"
	    ::trigger? ${dt}${line} $now
	} else {
	    ::outlog::puts $TS(outid) "$line"
	    ::trigger? $line $now
	}
    }
}


# Command Name     --  trigger?
# Original Author  --  Emmanuel Frecon - efrecon@gmail.com
#
# Consume one log line (timestamped) and check if it matches one of the
# registered patterns and, if so, if the trigger should be started.
#
# Arguments:
#    line	- Timestamped line of log
#    when	- Date contained in line, empty for now.
proc ::trigger? { line {when ""}} {
    global TS
    
    # Empty date is understood as current time in seconds.
    if { $when eq "" } {
	set when [clock seconds]
    }
    
    # Try matching the line against all existing triggers.
    foreach t [::uobj::allof [namespace current] "trigger"] {
	upvar \#0 $t TRG
	
	# Match the line against the pattern, based on its type: glob-style (the
	# default) or regexp.  The type is always in uppercase by construction.
	set modified 0
	switch -glob -- $TRG(-type) {
	    "R*" {
		if { [regexp $TRG(-ptn) $line] } {
		    lappend TRG(when) $when
		    set modified 1
		}
	    }
	    "G*" {
		if { [string match $TRG(-ptn) $line] } {
		    lappend TRG(when) $when
		    set modified 1
		}
	    }
	}
	
	# If the line matched, collect its timestamp and count number of
	# occurrences within the period set for the trigger. If the maximum was
	# reached, trigger.
	if { $modified } {
	    # Keep the list of occurences of the pattern within the specified
	    # period.
	    set kept [list]
	    foreach w $TRG(when) {
		if { $w >= $when-$TRG(-period) } {
		    lappend kept $w
		}
	    }
	    
	    # If maximum number of pattern occurrences is reached, call the
	    # trigger and start counting again.
	    if { [llength $kept] >= $TRG(-occurrences) } {
		set howlong [expr {[lindex $kept end] - [lindex $kept 0]}]
		set TRG(when) [list];  # Start counting from 0 again
		set cmd [linsert $TRG(-args) 0 \
				$TRG(-procedure) $TRG(-type) $TRG(-ptn) \
				[llength $kept] $howlong]
		$TS(log)::debug "Executing trigger $TRG(-procedure) as line\
		                 matches $TRG(-ptn)"
		if { [catch {$TRG(interp) eval $cmd} err] } {
		    $TS(log)::warn "Could not execute $TRG(-proc) in\
		                    $TRG(-source): $err"
		}
	    } else {
		# Just keep the latest relevant timestamps.
		set TRG(when) $kept
	    }
	}
    }
}



# Command Name     --  triggers
# Original Author  --  Emmanuel Frecon - efrecon@gmail.com
#
# Consume the specifications that were passed on the command-line (or read from
# the file) and create context for each trigger, including an interpreter
# wherein tcl code is being sourced.
#
# Arguments:
#    none
proc ::triggers { } {
    global TS

    foreach trg $TS(triggers) {
	# Extract all fields from the spec
	foreach {type ptn occurrences period spec} $trg break
	# When the proc is not given, it will default to "main"
	if { [string first @ $spec] >= 0 } {
	    foreach {p s} [split $spec @] break
	    set p [string trim $p]
	} else {
	    set p main
	    set s $spec
	}
	set s [::diskutil::fname_resolv [string trim $s]]
	if { [file readable $s] } {
	    # Create and initialise a context. We split the procedure
	    # specification along the ! sign, which can be used to pass
	    # parameters to a procedure.
	    set t [::uobj::new [namespace current] "trigger"]
	    upvar \#0 $t TRG
	    set TRG(-type) [string toupper $type]
	    set TRG(-ptn) $ptn
	    set TRG(-occurrences) $occurrences
	    set TRG(-period) $period
	    set call [split $p !]
	    set TRG(-procedure) [lindex $call 0]
	    set TRG(-args) [lrange $call 1 end]
	    set TRG(-source) $s
	    
	    # Initialise dynamic part of context, including the interpreter in
	    # which we source the code. Note that this is NOT a safe interpreter
	    # on purpose, so as to be able to have full power (incl. opening
	    # sockers or files).
	    set TRG(when) [list]
	    set TRG(interp) [interp create]
	    $TRG(interp) eval source $TRG(-source)
	} else {
	    $TS(log)::warn "Cannot access $s, ignoring trigger!"
	}
    }
}

# If -triggers is led by the @ sign, read the content from a file instead.
if { [string index $TS(triggers) 0] eq "@" } {
    set fname [string trim [string range $TS(triggers) 1 end]]
    set TS(triggers) [::diskutil::lread [::diskutil::fname_resolv $fname] 1 "Triggers"]
}

# Initialise all triggers, if any
::triggers

# Open output file for logging and start watching what is coming on the input
# file.
set TS(outid) [::outlog::open $TS(outfile) $TS(rotate) $TS(keep) $TS(maxsize)]
set TS(lid) [::logwatch::new $TS(infile) log_out]
if { $TS(endoneof) } {
	::logwatch::oneof $TS(lid) "exit"
}

vwait $TS(finish)
