##################
## Module Name     --  revive.tcl
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##    Traverse a directory tree and move back all "backup" files to their
##    original names.
##
##################
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.


# Array Name       --  REVIVE
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Global state of this program.
#
# Contents:
#    arg1	-
#    arg2	-
array set REVIVE {
    finished    0
}

source [file join [file dirname [info script]] .. bin argutil.tcl]
argutil::accesslib tcllib

# Now parse the options and put the result into the global state array
package require cmdline

set options {
    { backup.arg "(~|\\.bak)$" "Regular expression matching the backup files" }
    { verbose.arg "warn" "Verbosity Level" }
    { dirs.arg "" "List of directories" }
}

set inited [argutil::initargs REVIVE $options]
if { [catch {cmdline::typedGetoptions argv $options} optlist] != 0 } {
    puts [cmdline::typedUsage $options "accepts the following options:"]
    exit
}
array set REVIVE $optlist

foreach key $inited {
    argutil::makelist REVIVE($key)
}

# Include modules that we depend on.  This is complicated to be able
# to address separately modules in the verbose specification.
argutil::accesslib til
argutil::loadmodules [list diskutil synchro::file] $REVIVE(verbose)

# Initialise local logging facility
package require logger
set REVIVE(log) [::logger::init revive]
$REVIVE(log)::setlevel $REVIVE(verbose)


argutil::fix_outlog


proc dotraversal { dirtop } {
    global REVIVE

    # Get all readable files from the directory using a 8.3 compatible command
    set flist [glob -directory $dirtop -nocomplain -types r -- *]

    $REVIVE(log)::notice "Entering $dirtop..."
    foreach fname $flist {
	# Recurse into directories and make sure to
	# create information with the type of the path, the
	# relative name and its modification time.
	if { [file isdirectory $fname] } {
	    dotraversal $fname
	}
	if { [catch {regexp $REVIVE(backup) $fname} res] == 0 } {
	    set slash [string last "/" $fname]
	    if { $slash < 0 } { set slash 0 }
	    set newname [regsub -start $slash -- $REVIVE(backup) $fname ""]
	    $REVIVE(log)::debug "Moving $fname to $newname"
	    file rename -- $fname $newname
	} else {
	    $REVIVE(log)::warn \
		"Could not match $fname against $REVIVE(backup): $res"
	}
    }    
}

foreach dir $REVIVE(dirs) {
    set dir [::diskutil::fname_resolv $dir]
    dotraversal $dir
}


