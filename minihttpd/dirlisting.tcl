# dirlisting.tcl --
#
#	This modules provides a facility for directory content listing
#	within the HTTPD server.
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

package require Tcl 8.2
package require logger
package require html
package require textutil

namespace eval ::minihttpd::dirlist {}


# ::minihttpd::__dirlist_outfname -- Output one line of directory listing
#
#	This command formats one line for use by the directory
#	listing.  It is internal to the dirlisting algorithm but put
#	outside to simplify it visually.
#
# Arguments:
#	data_p	Pointer to variable to append line representation to
#	fname	Full path to name to add to line
#	date	Pretty printed date of file
#	size	Pretty printed size of file
#	maxnamesize	Maximum size of all names in directory
#	is_hdr	Is this the table header?
#
# Results:
#	None.
#
# Side Effects:
#	Modifies the variable pointed at by data_p
proc ::minihttpd::dirlist::__outfname { data_p fname date size maxnamesize { is_hdr 0 } } {
    upvar $data_p data

    # Name of file, if this is a real file and not the header of the
    # table, see to have a link to it.
    if { ! $is_hdr } {
	set outname [file tail $fname]
	if { [file isdirectory $fname] } {
	    append outname "/"
	}
	append data \
	    "[::html::openTag a href=\"[[namespace parent [namespace current]]::__StringToURL $outname]\"]"
	
    } else {
	set outname $fname
    }
    append data $outname
    if { ! $is_hdr } {
	append data "[::html::closeTag]"
    }
    
    append data \
	[string repeat " " \
	     [expr $maxnamesize - [string length $outname] + 2]]
    append data \
	[::textutil::adjust $date \
	     -full on -justify left -length 19 -strictlength on]
    append data \
	[::textutil::adjust $size \
	     -full on -justify right -length 10 -strictlength on]
    append data "\n"
}


# ::minihttpd::__friendly_datasize -- User friendly data size pretty format
#
#	This command provides a user friendly representation of a
#	number of bytes.  Bytes are automatically converted to Kbytes,
#	Mbytes, etc.
#
# Arguments:
#	size	Size to be pretty printed
#
# Results:
#	Returns a user friendly representation of the size.
#
# Side Effects:
#	None.
proc ::minihttpd::dirlist::__datasize { size } {
    if { $size < 1024 } {
	set sz [format "%7d" $size]
	return "$sz B "
    } elseif { $size < [expr 1024 * 1024] } {
	set sz [format "%.2f" [expr double($size) / 1024]]
	return "$sz KB"
    } elseif { $size < [ expr 1024 * 1024 * 1024] } {
	set sz [format "%.2f" [expr double($size) / 1024 / 1024 ]]
	return "$sz MB"	
    } elseif { $size < [ expr 1024 * 1024 * 1024 * 1024 ] } {
	set sz [format "%.2f" [expr double($size) / 1024 / 1024 / 1024]]
	return "$sz GB"
    } else {
	set sz [format "%.2f" \
		    [expr double($size) / 1024 / 1024 / 1024 / 1024]]
	return "$sz TB"
    }
}


# ::minihttpd::__dirlisting -- Create directory listing
#
#	This command prepares into the Client global array
#	representing clients the listing of a directory.  The output
#	is in HTML and is roughly similar to the one provided by
#	Apache, except that ordering is not supported.
#
# Arguments:
#	port	Port number of one of our HTTP servers.
#	sock	Socket to client.
#	dir	Directory to provide an HTML representation for
#	srvdir	Equivalent of that directory, relative to the server.
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::minihttpd::dirlist::dirlist { port sock dir srvdir } {
    set varname "[namespace parent [namespace current]]::Server_${port}"
    upvar \#0 $varname Server
	
    set idx [lsearch $Server(clients) $sock]
    if { $idx >= 0 } {
	set varname "[namespace parent [namespace current]]::Client_${port}_${sock}"
	upvar \#0 $varname Client

	set flist [glob -nocomplain -- [file join $dir "*"]]
	set maxsize [string length "Name"]
	foreach fname $flist {
	    set shortname [file tail $fname]
	    if { [expr [string length $shortname] + 2] > $maxsize } {
		set maxsize [expr [string length $shortname] + 2]
	    }
	}
	
	set Client(response) ""
	::html::init
	::html::title "Index of $srvdir"
	append Client(response) \
	    "[::html::head [list Index of $srvdir]]\n"
	append Client(response) "[::html::bodyTag]\n"
	
	append Client(response) "[::html::h1 [list Index of $srvdir]]\n"
	append Client(response) "[::html::openTag pre]\n"
	
	# Table header
	__outfname Client(response) "Name" "Date" "Size   " $maxsize 1
	append Client(response) "<hr />"

	# File listing.
	if { $srvdir != "/" } {
	    __outfname Client(response) ".." \
		[clock format [file mtime [file join $dir ".."]] \
		     -format "%d-%b-%Y %H:%M"] "-   " $maxsize
	}
	foreach fname [lsort $flist] {
	    if { [file isdirectory $fname] } {
		set sz "-   "
	    } else {
		set sz [__datasize [file size $fname]] 
	    }
	    __outfname Client(response) $fname \
		[clock format [file mtime $fname] -format "%d-%b-%Y %H:%M"] \
		$sz $maxsize
	}
	append Client(response) "<hr />"
	append Client(response) "[::html::closeTag]\n"
	
	append Client(response) "[::html::closeTag]\n"
	append Client(response) [::html::end]
    }
}

package provide minihttpd::dirlist 1.0