##################
## Module Name     --  cacheexplorer.tcl
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##   This utility rewriteexs the content of a cache (i.e. its index
##   file), replacing part of a URL by another string.
##
##################
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.


# Array Name       --  EXPLORER
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Global state of this program.
#
# Contents:
#    arg1	-
#    arg2	-
array set EXPLORER {
    finished    0
}

source [file join [file dirname $argv0] .. bin argutil.tcl]
argutil::accesslib tcllib

# Now parse the options and put the result into the global state array
package require cmdline

set options {
    { verbose.arg "warn" "Verbosity Level" }
    { cache.arg "" "Index of cache to explore" }
}

set inited [argutil::initargs EXPLORER $options]
if { [catch {cmdline::typedGetoptions argv $options} optlist] != 0 } {
    puts [cmdline::typedUsage $options "accepts the following options:"]
    exit
}
array set EXPLORER $optlist

foreach key $inited {
    argutil::makelist EXPLORER($key)
}

# Include modules that we depend on.  This is complicated to be able
# to address separately modules in the verbose specification.
argutil::accesslib til
argutil::loadmodules [list diskutil synchro::file] $EXPLORER(verbose)

# Initialise local logging facility
package require logger
set EXPLORER(log) [::logger::init explorer]
$EXPLORER(log)::setlevel $EXPLORER(verbose)


proc scrolled {type w args} {
    frame $w
    set res [eval $type $w.0\
		 [list -xscrollc "$w.x set" -yscrollc "$w.y set"] $args]
    scrollbar $w.x -ori hori -command "$w.0 xview"
    scrollbar $w.y -ori vert -command "$w.0 yview"
    grid $w.0 $w.y -sticky news
    grid $w.x      -sticky news
    grid columnconfigure $w 0 -weight 1
    grid rowconfigure    $w 0 -weight 1
    set res
}


proc explore { idx_fname } {
    global EXPLORER

    set tbl [scrolled ::tablelist::tablelist .tbl \
		 -columns {
		     0 "URL"
		     0 "Creation Date"
		     0 "Modification Date"
		     0 "Size"
		     0 "File Name"
		     0 "MIME Type" } -stretch all \
		 -labelcommand tablelist::sortByColumn \
		 -width 320 -height 240]

    pack .tbl -fill both -expand on

    # Replace all strings in the URLs of the index file
    if { [catch {open "$idx_fname"} fd] == 0 } {
	set i 0
	while { ! [eof $fd] } {
	    set line [gets $fd]
	    
	    if { [string trim $line] ne "" } {
		set url [lindex $line 0]
		set ctime [lindex $line 1]
		set mtime [lindex $line 2]
		set size [lindex $line 3]
		set fname [lindex $line 4]
		set mtype [lindex $line 5]
		
		$tbl insert end \
		    [list "$url" $ctime $mtime $size "$fname" "$mtype"]
	    }
	}		
	close $fd
    } else {
	$EXPLORER(log)::warn "Could not open index file for reading: $fd"
    }
}

argutil::fix_outlog

set idx [::diskutil::fname_resolv $EXPLORER(cache)]
if { [file exists $idx] } {
    package require Tk
    argutil::accesslib tablelist
    package require tablelist

    explore $idx
} else {
    $EXPLORER(log)::warn "$idx does not exist"
}

