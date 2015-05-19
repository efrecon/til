##################
## Module Name     --  cacheurl_rewrite.tcl
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##   This utility rewrites the content of a cache (i.e. its index
##   file), replacing part of a URL by another string.
##
##################
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.


# Array Name       --  REWRITE
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Global state of this program.
#
# Contents:
#    arg1	-
#    arg2	-
array set REWRITE {
    finished    0
}

source [file join [file dirname $argv0] .. bin argutil.tcl]
argutil::accesslib tcllib

# Now parse the options and put the result into the global state array
package require cmdline

set options {
    { verbose.arg "warn" "Verbosity Level" }
    { cacheindexes.arg "" "List of cache directories index" }
    { query.arg "" "Original part of URL to replace" }
    { replace.arg "" "Replacement string" }
}

set inited [argutil::initargs REWRITE $options]
if { [catch {cmdline::typedGetoptions argv $options} optlist] != 0 } {
    puts [cmdline::typedUsage $options "accepts the following options:"]
    exit
}
array set REWRITE $optlist

foreach key $inited {
    argutil::makelist REWRITE($key)
}

# Include modules that we depend on.  This is complicated to be able
# to address separately modules in the verbose specification.
argutil::accesslib til
argutil::loadmodules [list diskutil synchro::file] $REWRITE(verbose)

# Initialise local logging facility
package require logger
set REWRITE(log) [::logger::init rewrite]
$REWRITE(log)::setlevel $REWRITE(verbose)

proc do_replace { idx_fname } {
    global REWRITE

    # Replace all strings in the URLs of the index file
    set done 0
    if { [catch {open "$idx_fname"} old_fdes] == 0 } {
	if { [catch {open "${idx_fname}.new" "w"} new_fdes] == 0 } {
	    $REWRITE(log)::notice \
		"Replacing '$REWRITE(query)' by '$REWRITE(replace)'\
                 in $idx_fname"
	    while { ! [eof $old_fdes] } {
		set line [gets $old_fdes]
		
		set url [lindex $line 0]
		set ctime [lindex $line 1]
		set mtime [lindex $line 2]
		set size [lindex $line 3]
		set fname [lindex $line 4]
		set mtype [lindex $line 5]
		
		set idx [string first $REWRITE(query) $url]
		if { $idx >= 0 } {
		    set newurl [string range $url 0 [expr {$idx - 1}]]
		    append newurl $REWRITE(replace)
		    append newurl \
			[string range $url \
			     [expr {$idx + [string length $REWRITE(query)]}] \
			     end]
		    $REWRITE(log)::info "Rewriting '$url' to '$newurl'"
		    set done 1
		} else {
		    set newurl $url
		}
		puts $new_fdes \
		    "\"$newurl\" $ctime $mtime $size \"$fname\" \"$mtype\""
	    }
	    close $new_fdes
	} else {
	    $REWRITE(log)::warn \
		"Could not open temporary new index file ${idx_fname}.new\
                 for writing: $new_fdes"
	}
	close $old_fdes
    } else {
	$REWRITE(log)::warn "Could not open index file for reading: $old_fdes"
    }

    # Perform file name permutation if we actually perform one replace
    # operation, otherwise, simply get rid of the new file (which
    # would have the same content). Do permutation via another
    # temporary file to be sure we fail as soon as possible.
    if { $done } {
	file rename -force -- $idx_fname ${idx_fname}.old
	file rename -force -- ${idx_fname}.new $idx_fname
	file delete -force -- $idx_fname.old
    } elseif { [file exists ${idx_fname}.new] } {
	file delete -force -- ${idx_fname}.new
    }
}

argutil::fix_outlog

foreach idx $REWRITE(cacheindexes) {
    set dir [::diskutil::fname_resolv $idx]
    if { [file exists $idx] } {
	do_replace $idx
    } else {
	$REWRITE(log)::warn "$idx does not exist"
    }
}

