# uid.tcl -- Unique sting to identifier associations
#
#	This module implements a generic unique identifier generator.
#	For a given string value, this generator will always return
#	the same identifier.  These identifiers can be used to store
#	contexts in callers source, they are integers.  The generated
#	identifiers are unique within a context only and not in time
#	and space.
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

package require Tcl 8.2
package require logger

package provide uid 1.0

namespace eval ::uid {
    # Initialise global state
    variable UID
    if {![info exists UID]} {
	array set UID {
	    generators ""
	    id_gene    0
	    loglevel   warn
	}
	variable log [::logger::init uid]
	${log}::setlevel $UID(loglevel)
    }

    namespace export new id
}


# ::uid::loglevel -- Set/Get current log level.
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
proc ::uid::loglevel { { loglvl "" } } {
    variable UID
    variable log

    if { $loglvl != "" } {
	if { [catch "${log}::setlevel $loglvl"] == 0 } {
	    set UID(loglevel) $loglvl
	}
    }

    return $UID(loglevel)
}


# ::uid::new -- Create a new unique identifier context
#
#	This command creates a new unique identifier context within
#	which strings and associated identifiers will be allocated.
#
# Arguments:
#	args	list of options (-timeout -retries -redirects)
#
# Results:
#	A identifier for the context to be used in all further calls.
#
# Side Effects:
#	None.
proc ::uid::new { args } {
    variable UID
    variable log

    set id [incr UID(id_gene)]
    set varname "::uid::Generator_${id}"
    upvar \#0 $varname Generator
    set varname "::uid::Dict_${id}"
    upvar \#0 $varname Dict

    set Generator(id) $id
    set Generator(id_gene) 0
    foreach opt [array names UID "-*"] {
	set Generator($opt) $UID($opt)
    }

    lappend UID(generators) $id
    eval config $id $args

    return $id
}


# ::uid::id -- Return the unique identifier of a string
#
#	This command finds or creates the unique identifier associated
#	to a string within a given context and returns it.  The
#	identifier associated to the string will be unique within the
#	context and within this program, it is not unique in time and
#	space.
#
# Arguments:
#	id	Identifier of the context
#	str	String to find the unique identifier of
#
# Results:
#	The unique identifier of the string.
#
# Side Effects:
#	None.
proc ::uid::id { id str } {
    variable UID
    variable log

    set idx [lsearch $UID(generators) $id]
    if { $idx < 0 } {
	${log}::warn "$id does not identify a unique identifier generator"
	return -code error "Idenfier invalid"
    }
    
    set varname "::uid::Generator_${id}"
    upvar \#0 $varname Generator
    set varname "::uid::Dict_${id}"
    upvar \#0 $varname Dict

    # First try to find the identifier of the string
    if { [info exists Dict($str)] } {
	return $Dict($str)
    }
    
    # Otherwise generate a new identifier and store it.
    set uid [incr Generator(id_gene)]
    return [set Dict($str) $uid]
}



# ::uid::config -- Configure command result cacher.
#
#	This command set or get the options of a command result cache.
#
# Arguments:
#	id	Unique id generator.
#	args	list of options (-timeout -retries -redirects)
#
# Results:
#	Return all options, the option requested or set the options
#
# Side Effects:
#	None.
proc ::uid::config { id args } {
    variable UID
    variable log

    set idx [lsearch $UID(generators) $id]
    if { $idx < 0 } {
	${log}::warn "$id does not identify a unique identifier generator"
	return -code error "Idenfier invalid"
    }
    
    set varname "::uid::Generator_${id}"
    upvar \#0 $varname Generator

    set o [lsort [array names Generator "-*"]]

    if { [llength $args] == 0 } {      ;# Return all results
	set result ""
	foreach name $o {
	    lappend result $name $Generator($name)
	}
	return $result
    }

    foreach {opt value} $args {        ;# Get onr or set some
	if { [lsearch $o $opt] == -1 } {
	    return -code error "Unknown option $opt, must be: [join $o ", " ]"
	}
	if { [llength $args] == 1 } {  ;# Get one config value
	    return $Generator($opt)
	}
	set Generator($opt) $value         ;# Set the config value
	set rehash 1
    }
}


# ::uid::defaults -- Set/Get defaults for all new result cacher
#
#	This command sets or gets the defaults opetions for all new
#	command result cacher, it will not perpetrate on existing
#       commands, use ::uid::config instead.
#
# Arguments:
#	args	List of -key value or just -key to get value
#
# Results:
#	Return all options, the option requested or set the options
#
# Side Effects:
#	None.
proc ::uid::defaults { args } {
    variable UID
    variable log

    set o [lsort [array names UID "-*"]]

    if { [llength $args] == 0 } {      ;# Return all results
	set result ""
	foreach name $o {
	    lappend result $name $UID($name)
	}
	return $result
    }

    foreach {opt value} $args {        ;# Get onr or set some
	if { [lsearch $o $opt] == -1 } {
	    return -code error "Unknown option $opt, must be: [join $o ,]"
	}
	if { [llength $args] == 1 } {  ;# Get one config value
	    return $UID($opt)
	}
	set UID($opt) $value           ;# Set the config value
    }
}
