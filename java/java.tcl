# java.tcl --
#
#	This module contains a number of java interfacing utilities
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

package require Tcl 8.2
package require logger

package provide java 1.0

namespace eval ::java {
    # Initialise global state
    variable JAVA
    if {![info exists JAVA]} {
	array set JAVA {
	    utf_socks     ""
	    loglevel      warn
	}
	variable log [::logger::init java]
	${log}::setlevel $JAVA(loglevel)
    }

    # Export commands.
    namespace export find \
	utfsock_init utfsock_close utfsock_open utfsock_write utfsock_read
}



# ::java::loglevel -- Set/Get current log level.
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
proc ::java::loglevel { { loglvl "" } } {
    variable JAVA
    variable log

    if { $loglvl != "" } {
	if { [catch "${log}::setlevel $loglvl"] == 0 } {
	    set JAVA(loglevel) $loglvl
	}
    } 

    return $JAVA(loglevel)
}



# ::java::utfsock_init --
#
#	Initialise a Tcl stream socket to be read and write using
#	readUTF() and writeUTF() compliant procedures.  The socket
#	should then be read, written and closed using the commands
#	provided by this module.
#
# Arguments:
#	sock	Opened socket to initialise
#
# Results:
#	None.
#
# Side Effects:
#	This command modifies the translation and encoding settings of
#	the socket.
proc ::java::utfsock_init { sock } {
    variable JAVA
    variable log

    lappend JAVA(utf_socks) $sock
    fconfigure $sock -translation binary -encoding binary
    ${log}::debug "Socket $sock initialised for java UTF compatible operations"
}


# ::java::utfsock_close --
#
#	Close a socket unless it was not one of our sockets registered
#	sockets.
#
# Arguments:
#	sock	Socket to close
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::java::utfsock_close { sock } {
    variable JAVA
    variable log

    set idx [lsearch $JAVA(utf_socks) $sock]
    if { $idx >= 0 } {
	set JAVA(utf_socks) [lreplace $JAVA(utf_socks) $idx $idx]
	close $sock
	${log}::debug "Java socket $sock is now closed"
    } else {
	${log}::warn "$sock was not registered as a Java-compatible socket"
    }
}



# ::java::utfsock_write --
#
#	Write a string to a Java UTF-8 initialised socket.  Performs
#	automatic initialisation if the socket has not already been
#	initialised.
#
# Arguments:
#	sock	Socket to write to.
#	str	String to write to the socket.
#
# Results:
#	A positive number on success, 0 on failure when writing and a
#	negative number when the length of the string to write would
#	overcome the maximum possible size.
#
# Side Effects:
#	None.
proc ::java::utfsock_write { sock str } {
    variable JAVA
    variable log

    # Initialise socket if necessary
    set idx [lsearch $JAVA(utf_socks) $sock]
    if { $idx < 0 } {
	${log}::info \
	    "Automatically initialising socket $sock for java UTF operation"
	::java::utfsock_init $sock
    }

    # Read in a Java compliant manner and return appropriate value
    set data [encoding convertto utf-8 $str]
    if {[string length $data] <= 0xffff} {
	set len [binary format S [string length $data]]
	puts -nonewline $sock $len$data
	set res [catch "flush $sock"]
	if { $res == 0 } {
	    ${log}::debug
		"Java wrote length: [string length $data] and data $str"
	}
	return $res
    } else {
	return -1
    }
}



# ::java::utfsock_read --
#
#	Read a string from a Java UTF-8 initialised socket.  Mimic
#	gets behaviour.
#
# Arguments:
#	sock	Socket to read from
#	var_p	Variable to store data in
#
# Results:
#	Return ths string or the total number of bytes read, depending
#	on the number of arguments.
#
# Side Effects:
#	None.
proc ::java::utfsock_read { sock { var_p "" } } {
    variable JAVA
    variable log

    # Initialise socket if necessary
    set idx [lsearch $JAVA(utf_socks) $sock]
    if { $idx < 0 } {
	${log}::info \
	    "Automatically initialising socket $sock for java UTF operation"
	::java::utfsock_init $sock
    }

    # Arrange to point at variable if necessary.
    if { $var_p != "" } {
	upvar $var_p str
    }
    
    # Read in a Java compliant manner
    set res [catch "read $sock 2" len]
    if { $res != 0 } {
	if { $var_p == "" } {
	    return ""
	} else {
	    return -1
	}
    }
    binary scan $len S length
    if { [info vars length] != "length" } {
	if { $var_p == "" } {
	    return ""
	} else {
	    return -1
	}
    }
    ${log}::debug "Java read message is $length bytes long"
    set res [catch "read $sock [expr {$length & 0xffff}]" data]
    if { $res != 0 } {
	if { $var_p == "" } {
	    return ""
	} else {
	    return -1
	}
    }
    set str [encoding convertfrom utf-8 $data]
    ${log}::debug "Java read message contains $str"

    # Return correct value
    if { $var_p == "" } {
	return $str
    } else {
	return [expr $length + 2]
    }
}



