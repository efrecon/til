# alloc.tcl --
#
#	This module handles port allocation for a number of
#	(distributed) services in a deterministic manner.  It
#	associates services to ports and arranges to avoid that two
#	different services will share the same port.  Port allocation
#	is based on a random but deterministic manner.
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

package require Tcl 8.2
package require logger
package require md5
package require textutil

package provide portsalloc 1.0

namespace eval ::portsalloc {
    # Initialise global state
    variable PA
    if {![info exists PA]} {
	array set PA {
	    services    ""
	    -portmin    2000
	    -portmax    6000
	    db          ""
	    loglevel    warn
	    seed        ""
	}
	variable log [::logger::init [string trimleft [namespace current] ::]]
	${log}::setlevel $PA(loglevel)
    }

    namespace export loglevel find_byname find_byport listall add init
}


# ::portsalloc::loglevel -- Set/Get current log level.
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
proc ::portsalloc::loglevel { { loglvl "" } } {
    variable PA
    variable log

    if { $loglvl != "" } {
	if { [catch "${log}::setlevel $loglvl"] == 0 } {
	    set PA(loglevel) $loglvl
	}
    }

    return $PA(loglevel)
}


# ::portsalloc::__srand -- Initialise local pseudo-random sequence
#
#	This procedure initialises the local pseudo-random sequence
#	and act similarily as srand().
#
# Arguments:
#	seed	Seed to initialise with
#
# Results:
#	Return a number between 0.0 and 1.0
#
# Side Effects:
#	None.
proc ::portsalloc::__srand { { seed "" } } {
    variable PA
    variable log

    if { $seed == "" } {
	set seed [expr {[clock clicks] + [pid]}]
    }
    set PA(seed) [expr {$seed % 259200}]
    return [expr {double($PA(seed)) / 259200}]
}



# ::portsalloc::__rand -- Return next pseudo-random in sequence
#
#	This procedure computes the next pseudo-random number from the
#	sequence and returns it.
#
# Arguments:
#	None.
#
# Results:
#	Return a number between 0.0 and 1.0
#
# Side Effects:
#	None.
proc ::portsalloc::__rand { } {
    variable PA
    variable log

    if { $PA(seed) == "" } {
	return [__srand]
    }

    set PA(seed) [expr {($PA(seed) * 7141 + 54773) % 259200}]
    return [expr {double($PA(seed)) / 259200}]
}



# ::portsalloc::__allocate -- Allocate a port number
#
#	This procedure allocates a new port number given the current
#	state of affairs and its name.  The procedure hashes the name
#	to attempt to extract a viable port from the sub-parts of the
#	hashes.  If not possible, it initialises the random sequence
#	using the hash and tries to look from an empty slot.  This
#	ensures that given the same state of affairs, the same port
#	number will always be returned across time.
#
# Arguments:
#	fname	Name of file (defaults to current db)
#
# Results:
#	Return 1 on success, 0 otherwise
#
# Side Effects:
#	None.
proc ::portsalloc::__allocate { name } {
    variable PA
    variable log

    set period [expr $PA(-portmax) - $PA(-portmin)]

    # First try through using the direct result of hashing on the name
    ${log}::info "Try allocating $name through MD5 hashing"
    set hash [::md5::md5 -hex $name]
    for { set i 0 } { $i < [string length $hash] } { incr i 4 } {
	set hex [string range $hash $i [expr $i + 3]]
	set port [expr $PA(-portmin) + [scan $hex "%4x"] % $period]
	if { [llength [find_byport $port]] > 0 } {
	    return $port
	}
    }

    # If that did not succeed, initiate the random sequence using the
    # hash and loop from there until we find an available port number.
    ${log}::info "Try allocating $name through following random sequence"
    set port [expr {$PA(-portmin) \
			+ int([__srand [scan [string range $hash 0 7] "%4x"]] \
				  * $period)}]
    while { [llength [find_byport $port]] > 0 } {
	set port [expr {$PA(-portmin) + int([__rand] * $period)}]
    }

    return $port
}



# ::portsalloc::__writedb -- Write state to file
#
#	This procedure writes the current ports description database
#	to a file.
#
# Arguments:
#	fname	Name of file (defaults to current db)
#
# Results:
#	Return 1 on success, 0 otherwise
#
# Side Effects:
#	None.
proc ::portsalloc::__writedb { { fname "" } } {
    variable PA
    variable log

    if { $fname == "" } {
	set fname $PA(db)
    }

    # Still no file to write, no persistence possible
    if { $fname == "" } {
	return 0
    }

    if { [catch {open $fname w} fdes] == 0 } {
	set maxsize 0
	foreach pdesc $PA(services) {
	    set name [lindex $pdesc 0]
	    if { [string length $name] > $maxsize } {
		set maxsize [string length $name]
	    }
	}
	incr maxsize

	foreach pdesc $PA(services) {
	    puts -nonewline $fdes \
		[::textutil::adjust [lindex $pdesc 0] \
		     -full on -justify left -length $maxsize -strictlength on]
	    puts -nonewline $fdes \
		[::textutil::adjust [lindex $pdesc 1] \
		     -full on -justify right -length 5 -strictlength on]
	    puts $fdes " \"[lindex $pdesc 2]\""
	}
	close $fdes
	${log}::notice "Wrote service description file to $fname"
	return 1
    } else {
	${log}::warn "Could not open file $fname for writing: $fdes"
    }

    return 0
}


# ::portsalloc::__readdb -- Merge service allocation file
#
#	This procedure reads a ports description database and append
#	it to the current database.  The appending operation does not
#	take care of port duplicates that might occur when appending.
#
# Arguments:
#	fname	Name of file (defaults to current db)
#
# Results:
#	Return the number of read entries, -1 on errors.
#
# Side Effects:
#	None.
proc ::portsalloc::__readdb { { fname "" } } {
    variable PA
    variable log

    if { $fname == "" } {
	set fname $PA(db)
    }

    # Still no file to read from, no persistence.
    if { $fname == "" } {
	return 0
    }

    ${log}::notice "Reading ports description file $fname"
    set nbread 0
    if { [catch "open $fname" fd] == 0 } {
	while { ![eof $fd] } {
	    set line [string trim [gets $fd]]
	    if { $line != "" } {
		set firstchar [string index $line 0]
		if { $firstchar != "\#" && $firstchar != ";" } {
		    set srv ""
		    foreach item $line {
			if { $item != "" } {
			    lappend srv $item
			}
		    }
		    if { [llength $srv] == 2 } {
			lappend srv ""
		    }
		    lappend PA(services) $srv
		    incr nbread
		}
	    }
	}
	close $fd

	return $nbread
    } else {
	${log}::warn "Could not find ports description file $fname: $fd"
    }

    return -1
}



# ::portsalloc::find_byname -- Find service information
#
#	This commands finds the port of a service given its name.
#
# Arguments:
#	name	Name of the service to look for.
#
# Results:
#	Return the port of the service if it is known, otherwise -1.
#
# Side Effects:
#	None.
proc ::portsalloc::find_byname { name } {
    variable PA
    variable log

    foreach pdesc $PA(services) {
	if { $name == [lindex $pdesc 0] } {
	    return [lindex $pdesc 1]
	}
    }

    return -1
}



# ::portsalloc::find_byport -- Find port information
#
#	This commands finds information for a known port and returns it.
#
# Arguments:
#	port	Port to look information for.
#
# Results:
#	Return a list composed of the name of the service and its
#	textual description if the port is known, an empty list
#	otherwise.
#
# Side Effects:
#	None.
proc ::portsalloc::find_byport { port } {
    variable PA
    variable log

    foreach pdesc $PA(services) {
	if { $port == [lindex $pdesc 1] } {
	    return [list [lindex $pdesc 0] [lindex $pdesc 2]]
	}
    }

    return {}
}


# ::portsalloc::listall -- List all allocated ports
#
#	This commands lists all the currently allocated ports for the
#	known services.
#
# Arguments:
#	None.
#
# Results:
#	Return the list of allocated ports.
#
# Side Effects:
#	None.
proc ::portsalloc::listall { } {
    variable PA
    variable log

    set ports ""
    foreach pdesc $PA(services) {
	lappend ports [lindex $pdesc 1]
    }

    return $ports
}



# ::portsalloc::add -- Allocate and add a port for a service
#
#	This commands allocates and add a port for a service and makes
#	sure to store it to store into the persistent database if
#	possible.
#
# Arguments:
#	name	Name of service
#	desc	Short textual description for service.
#
# Results:
#	Return the port allocated to the service
#
# Side Effects:
#	Write to persistent ports file if possible.
proc ::portsalloc::add { name { desc "" } } {
    variable PA
    variable log

    set port [find_byname $name]
    if { $port < 0 } {
	set port [__allocate $name]
	lappend PA(services) [list $name $port "$desc"]
	__writedb
	${log}::notice "'$name' associated to port $port"
    }

    return $port
}


# ::portsalloc::init -- Initialise persistent allocation
#
#	Initialises the library through giving it the name of the file
#	that will act as a database for the module and for persistent
#	allocation between uses.  If no file is given, allocation will
#	not be able to persist with time.
#
# Arguments:
#	fname	Name of file
#
# Results:
#	Return the current list of allocated ports.
#
# Side Effects:
#	None.
proc ::portsalloc::init { { fname "" } } {
    variable PA
    variable log

    if { $fname != "" } {
	set PA(db) [file normalize $fname]
	__readdb
    }

    return [listall]
}


