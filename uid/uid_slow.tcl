# uid.tcl -- Unique sting to identifier associations
#
#	This module implements a generic unique identifier generator.
#	For a given string value, this generator will always return
#	the same identifier.  These identifiers can be used to store
#	contexts in callers source, they are integers.  The generated
#	identifiers are unique within a context only and not in time
#	and space.  The module support sparse and dense contexts
#	through the possibility (or not) to hash strings when storing
#	them and their corresponding identifiers.  When many strings
#	are to be stored, hashing (a positive integer) is recommended.
#	When few strings are anticipated, linear storage (any negative
#	integer) is recommended.
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
	    -hash      20
	    -hashstart 0
	    -hashstop  end
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

    set Generator(id) $id
    set Generator(id_gene) 0
    foreach opt [array names UID "-*"] {
	set Generator($opt) $UID($opt)
    }

    lappend UID(generators) $id
    eval config $id $args

    return $id
}


# ::uid::__find -- Find the identifier of a string
#
#	This command finds the identifier associated to a string
#	within a given context.
#
# Arguments:
#	id	Identifier of the context
#	str	String to find in context
#
# Results:
#	The identifier of the string or an empty string if not found
#
# Side Effects:
#	None.
proc ::uid::__find { id str } {
    variable UID
    variable log

    set varname "::uid::Generator_${id}"
    upvar \#0 $varname Generator

    # Find either linearily through all existing strings, or via
    # hashing the string and its hash storage.
    if { $Generator(-hash) <= 0 } {
	if { [array names Generator strings] eq "" } {
	    return ""
	}
	set idx [lsearch -exact $Generator(strings) $str]
	if { $idx >= 0 } {
	    return [lindex $Generator(identifiers) $idx]
	} else {
	    return ""
	}
    } else {
	set hash [__hash $id $str]
	if { [array names Generator strings_$hash] eq "" } {
	    return ""
	}
	set idx [lsearch -exact $Generator(strings_$hash) $str]
	if { $idx >= 0 } {
	    return [lindex $Generator(identifiers_$hash) $idx]
	} else {
	    return ""
	}
    }

    return ""; #Never reached.
}


# ::uid::__hash -- Hash a string
#
#	This command computes the hash of a string within a given context
#
# Arguments:
#	id	Identifier of the context
#	str	String to compute the hash of
#
# Results:
#	The hash value of the string.
#
# Side Effects:
#	None.
proc ::uid::__hash { id str } {
    variable UID
    variable log

    set varname "::uid::Generator_${id}"
    upvar \#0 $varname Generator
    
    # Extract the substring to hash, as from configuration
    set hashstr [string range $str \
		     $Generator(-hashstart) $Generator(-hashstop)]
    # Compute the hash by xor-ing the values of the characters of the
    # substring, starting at 0 to make sure we always have at least
    # one hash.
    set hash 0
    set len [string length $hashstr]
    for { set i 0 } { $i < $len } { incr i } {
	set val [scan [string index $hashstr $i] %c]
	set hash [expr {($hash + $val) % $Generator(-hash)}]
    }

    ${log}::debug "Hashed $str to $hash"
    return $hash
}


# ::uid::__store -- Store unique identifier for string in context
#
#	This command stores the unique identifier associated to a
#	string in a given context with respect to the current hashing
#	technique and hashing value.
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
proc ::uid::__store { id uid str } {
    variable UID
    variable log

    set varname "::uid::Generator_${id}"
    upvar \#0 $varname Generator
    
    if { $Generator(-hash) <= 0 } {
	lappend Generator(strings) "$str"
	lappend Generator(identifiers) $uid
	${log}::debug "Linarily stored $str with unique id $uid"
    } else {
	set hash [__hash $id $str]
	lappend Generator(strings_$hash) "$str"
	lappend Generator(identifiers_$hash) $uid
	${log}::debug "Stored $str with unique id $uid (hash: $hash)"
    }
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

    # First try to find the identifier of the string
    set uid [__find $id $str]
    
    # If we could not create one, and handle the value of the -hash
    # configuration option correctly.  A negative value means linear
    # storage of all strings and is appropriate when there will only
    # be a few strings, a positive value will compute a hash for the
    # string and will lead to quicker search time when many strings
    # are to be associated to unique identifiers.
    if { $uid eq "" } {
	set uid [incr Generator(id_gene)]
	__store $id $uid $str
    }

    return $uid
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

    set rehash 0
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

    if { $rehash } {
	# Gather all currently known identifiers and their strings in
	# the strings and identifiers variables.  Empty the current
	# context from its content at the same time.
	set strings ""
	set identifiers ""
	if { [array names Generator strings_*] ne "" } {
	    foreach k [array names Generator strings_*] {
		set hash [regsub "strings_" $k ""]
		set strings [concat $strings $Generator(strings_$hash)]
		set identifiers [concat $identifiers \
				     $Generator(identifiers_$hash)]
		unset Generator(strings_$hash)
		unset Generator(identifiers_$hash)
	    }
	} elseif { [array names Generator strings] ne "" } {
	    set strings $Generator(strings)
	    set identifiers $Generator(identifiers)
	    unset Generator(strings)
	    unset Generator(identifiers)
	}
	
	# And re-store them in the context according to the (new)
	# values for the hashing technique.
	set len [llength $identifiers]
	for { set i 0 } { $i < $len } { incr i } {
	    __store $id [lindex $identifiers $i] [lindex $strings $i]
	}
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
