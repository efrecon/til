# file.tcl --
#
#	This modules provides the http file synchronisation
#	implementation.
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

package require Tcl 8.3
package require logger
package require http
package require uri
package require massgeturl
package require diskutil


namespace eval ::synchro::http {
    variable HTTP
    if {![::info exists HTTP]} {
	array set HTTP {
	    idgene        0
	    loglevel      "warn"
	    pending       ""
	}
	variable log [::logger::init [string trimleft [namespace current] ::]]
	${log}::setlevel $HTTP(loglevel)
    }

    namespace export loglevel traverse geturl cancel
}


# ::synchro::http::loglevel -- Set/Get current log level.
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
proc ::synchro::http::loglevel { { loglvl "" } } {
    variable HTTP
    variable log

    if { $loglvl != "" } {
	if { [catch "${log}::setlevel $loglvl"] == 0 } {
	    set HTTP(loglevel) $loglvl
	}
    }

    return $HTTP(loglevel)
}


# ::synchro::http::__dateguess -- Guess a date from a string
#
#	This command trims a string from the end and start, until it
#	finds a string that is compatible with the clock scan format.
#	The best bet for the date is returned, i.e. the successfull
#	date that was computed from the longest strings.  This
#	commands trims on spaces, and thus requires words in the input
#	string.
#
# Arguments:
#	str	Input string
#
# Results:
#	Returns the best date (as an integer) that could be extracted
#	from the string.
#
# Side Effects:
#	None.
proc ::synchro::http::__dateguess { str {tails {end start}}} {
    variable HTTP
    variable log

    # Initialise results array to 0
    set tails [string tolower $tails]
    array set results {
	start    "0 0"
	end      "0 0"
    }

    # Now start trimming the input string on words, from the start and
    # then from the end.
    foreach t $tails {
	# Reinitialise status at each iteration.
	set dt 0
	set teststr $str

	# Trim as long as we don't have a date
	while { $dt == 0 && $teststr ne "" } {
	    # Try scanning the string.
	    if { [catch {clock scan $teststr} dt] } {
		# If we could not scan the string, trim it from start
		# or end, until we find a space.
		set dt 0
		if { $t eq "end" } {
		    while { $teststr != "" } {
			set teststr [string range $teststr 0 end-1]
			if { [string is space \
				  [string index $teststr end]] } {
			    break
			}
		    }
		} elseif { $t eq "start" } {
		    while { $teststr != "" } {
			set teststr [string range $teststr 1 end]
			if { [string is space \
				  [string index $teststr 0]] } {
			    break
			}
		    }
		}
	    } else {
		# Store success if we could scan the string
		set results($t) [list [string length $teststr] $dt]
		${log}::debug "Matched '$teststr' to $dt (input was: $str)"
	    }
	}
    }

    if { [lindex $results(end) 0] > [lindex $results(start) 0] } {
	return [lindex $results(end) 1]
    } else {
	return [lindex $results(start) 1]
    }
}



# ::synchro::http::get_links -- Get links from XHTML
#
#	Parses a string containing HTML code and return a list of all
#	the links pointed at in the document.  This command tries to
#	interpret the results of HTML listing of directories in a
#	flexible way.
#
# Arguments:
#	str	XML content of document.
#
# Results:
#	List of pairs containing the link and its modification date.
#
# Side Effects:
#	None.
proc ::synchro::http::__get_links { str } {
    set len [string length $str]

    set links ""

    for { set i 0 } { $i < $len } { incr i } {
	set char [string index $str $i]
	if { $char == "<" } {
	    if { [string equal -nocase \
		      [string range $str $i [expr $i + 1]] "<A"] } {
		# We have found an A tag, parse it until its end.
		set j $i
		while { $j < $len } {
		    set close [string index $str $j]
		    if { $close == ">" } {
			break
		    }
		    incr j
		}
		
		# Now look for the source URL within the tag.
		set tag [string range $str $i $j]
		set uppertag [string toupper $tag]
		set src [string first "HREF" $uppertag]
		set url ""
		if { $src > 0 } {
		    set openquote [string first "\"" $tag $src]
		    set closequote [string first "\"" $tag \
					[expr $openquote + 1]]
		    set url [string trim \
				 [string range $tag \
				      [expr $openquote + 1] \
				      [expr $closequote -1]]]
		}
		
		if { $url ne "" } {
		    set endtag [string first "</" $str $j]
		    if { $endtag >= 0 } {
			set fname [string range $str $j $endtag]
			set fname [string trim [string range $fname 1 end-1]]
		    }

		    # We want to find the name of the file in the link
		    # to be sure it comes from there.
		    if { [string first $fname $url] >= 0 } {
			# Now try to extract size and modification time...
			set close [string first ">" $str [expr $endtag + 1]]
			incr close
			set nextopen [string first "<" $str [expr $close +1]]
			incr nextopen -1

			set info [string range $str $close $nextopen]
			set dtstr [regsub {(\d+\s*bytes)|(\s+dir\s+)|(\s+-\s*)|(\d+(\.\d+)?\s?(B|K|M|G)?\s*$)} $info ""]
			set dt [__dateguess $dtstr]
			
			lappend links $url $dt
		    }
		}
	    }
	}
    }

    return $links
}


# ::synchro::http::__traverse -- Traversal of remote directories
#
#	This command will perform HTTP traversal of directories.
#	These have to be served by HTTP servers that send back HTML
#	listings of directory content.  See to remove the top
#	traversal directory from the file names being stored so that
#	all names are relative.
#
# Arguments:
#	dirtop	HTTP Directory to traverse
#	strip	Directory information to strip, i.e. origin top directory
#	consider	List of regular expressions for file names to consider
#	ignore	List of regular expressions for file names to ignore
#
# Results:
#	Return the list of (relative) directories and files.
#
# Side Effects:
#	None.
proc ::synchro::http::__traverse {dirtop strip {consider {".*"}} {ignore {}}} {
    variable HTTP
    variable log

    ${log}::debug "Entering '$dirtop'"
    set token [::http::geturl $dirtop]
    upvar \#0 $token uinfo

    set reslist [list]
    if { $uinfo(status) == "ok" } {
	set links [__get_links $uinfo(body)]
	foreach {l dt} $links {
	    if { $l ne "." && $l ne ".." && $l ne "./" && $l ne "../" } {
		set lnk [::uri::resolve $dirtop $l]
		set rname [string range $lnk [string length $strip] end]
		if { [::diskutil::match $rname $consider $ignore] } {
		    if { [string index $lnk end] == "/" } {
			lappend reslist [list "DIR" $rname $dt]
			foreach sub \
			    [__traverse $lnk $strip $consider $ignore] {
				lappend reslist $sub
			    }
		    } else {
			lappend reslist [list "FILE" $rname $dt]
		    }
		}
	    }
	}
    }

    return $reslist
}


# ::synchro::http::traverse -- Traverse HTTP directory structure
#
#	Traverse HTTP directory structure.
#
# Arguments:
#	dirtop	Top directory to traverse
#	consider	List of regular expressions for file names to consider
#	ignore	List of regular expressions for file names to ignore
#
# Results:
#	Return the list of (relative) directories and files.
#
# Side Effects:
#	None.
proc ::synchro::http::traverse { dirtop { consider {".*"} } { ignore {} } } {
    variable HTTP
    variable log
    
    set flist [__traverse $dirtop $dirtop $consider $ignore]

    return $flist
}


# ::synchro::http::__progress -- Deliver progress report
#
#	Bridge progress report from massgeturl to the synchro syntax
#
# Arguments:
#	arg1	descr1
#	arg2	descr2
#
# Results:
#	None
#
# Side Effects:
#	None.
proc ::synchro::http::__progress { pgesscb dst_fp id furl url current total} {
    variable HTTP
    variable log

    if { $pgesscb ne "" } {
	if { [catch {eval $pgesscb $id \$dst_fp} res] } {
	    ${log}::warn "Error when invoking progress callback: $res"
	}
    }
    
}


# ::synchro::http::__delivery -- Deliver success/failure report
#
#	Bridge progress report from massgeturl to the synchro syntax
#
# Arguments:
#	arg1	descr1
#	arg2	descr2
#
# Results:
#	None
#
# Side Effects:
#	None.
proc ::synchro::http::__delivery { donecb dst_fp id url status tok } {
    variable HTTP
    variable log

    if { $status == "OK" } { set tok "" }
    if { [catch {eval $donecb $url \$dst_fp $status $tok} res] } {
	${log}::warn "Error when invoking done callback: $res"
    }
}


# ::synchro::http::cancel -- Cancel an HTTP copy
#
#	This command cancels a copy in progress, if any and possible.
#	It will call the callbacks with "CANCEL" as an argument.
#
# Arguments:
#	id	Identifier of copy
#
# Results:
#	Boolean telling whether we have cancelled the copy or not.
#
# Side Effects:
#	Will remove the destination file!
proc ::synchro::http::cancel { id } {
    ::massgeturl::cancel $id
}


# ::synchro::http::geturl -- Copy a file
#
#	This command will copy a file from another directory into a
#	destination file name.  A callback will be invoked once the
#	copy has ended or has failed.  The callback will be callbed
#	with the following arguments: the full name of the source
#	file, the path to the destination file, a status code (OK or
#	ERROR) and, in case of errors, some additional text explaining
#	the error.  This command perform the copy in the background
#	and is therefor able to handle several calls with the same
#	source and destinations gracefully (i.e. through only copying
#	once!).
#
# Arguments:
#	dirtop	Top directory under which the source file is.
#	src	Relative path to the source file.
#	dst_fp	Name of destination file.
#	donecb	Command to call back at completion.
#	bytes	How many bytes to copy from file?
#
# Results:
#	An identifier of the copy operation or empty string on error.
#
# Side Effects:
#	Will copy file on the local hard drive!
proc ::synchro::http::geturl { dirtop src dst_fp donecb {pgesscb ""} {bytes -1}} {
    variable HTTP
    variable log

    if { $bytes >= 0 } {
	return -code error "Partial downloading not yet implemented!"
    }

    set src_fp $dirtop
    if { [string index $src_fp end] != "/" } { append src_fp "/" }
    append src_fp $src

    set id [::massgeturl::infile $src_fp $dst_fp \
		[list ::synchro::http::__delivery "$donecb" "$dst_fp"] \
		-progress [list ::synchro::http::__progress "$pgesscb" "$dst_fp"]]

    return $id
}


package provide synchro::http 1.0
