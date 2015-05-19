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
argutil::loadmodules [list outlog diskutil logwatch] $TS(verbose)


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
	if { [catch {clock scan $dt}] == 0 } {
	    set dumpdate 0
	}
    }

    if { $dump } {
	if { $dumpdate && $TS(dateformat) != "" } {
	    set dt "[clock format [clock seconds] -format $TS(dateformat)]"
	    ::outlog::puts $TS(outid) "${dt}${line}"
	} else {
	    ::outlog::puts $TS(outid) "$line"
	}
    }
}


set TS(outid) [::outlog::open $TS(outfile) $TS(rotate) $TS(keep) $TS(maxsize)]
set TS(lid) [::logwatch::new $TS(infile) log_out]
if { $TS(endoneof) } {
	::logwatch::oneof $TS(lid) "exit"
}

vwait $TS(finish)
