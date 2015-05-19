# rescacher.tcl --
#
#	This module implements a generic result cacher module that
#	will cache the result of commands that take time and return
#	the same result for a given set of arguments.
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

package require Tcl 8.2
package require logger

package provide rescacher 1.0

namespace eval ::rescacher {
    # Initialise global state
    variable RC
    if {![info exists RC]} {
	array set RC {
	    cmds       ""
	    id_gene    0
	    loglevel   warn
	    -maxstore  20
	}
	variable log [::logger::init rescacher]
	${log}::setlevel $RC(loglevel)
    }

    namespace export call config defaults
}


# ::rescacher::loglevel -- Set/Get current log level.
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
proc ::rescacher::loglevel { { loglvl "" } } {
    variable RC
    variable log

    if { $loglvl != "" } {
	if { [catch "${log}::setlevel $loglvl"] == 0 } {
	    set RC(loglevel) $loglvl
	}
    }

    return $RC(loglevel)
}


proc ::rescacher::__cmd_store { cmd } {
    variable RC
    variable log

    foreach {id c} $RC(cmds) {
	if { $c eq $cmd } {
	    return "::rescacher::Command_${id}"
	}
    }

    set id [incr RC(id_gene)]
    set varname "::rescacher::Command_${id}"
    upvar \#0 $varname Command

    set Command(id) $id
    set Command(cmd) $cmd
    set Command(cache) ""
    foreach opt [array names RC "-*"] {
	set Command($opt) $RC($opt)
    }
    
    ${log}::info "Adding new command $cmd to result cacher"
    lappend RC(cmds) $id "$cmd"

    return $varname
}


proc ::rescacher::call { args } {
    variable RC
    variable log

    upvar \#0 [__cmd_store [lindex $args 0]] Command

    # Return value if already in cache.
    set idx 0
    foreach {res params} $Command(cache) {
	if { $params eq [lrange $args 1 end] } {
	    # Move the command and result at the end of the list to
	    # ensure newest commands called will not be removed.
	    set Command(cache) [lreplace $Command(cache) \
				    [expr {2*$idx}] [expr {1+2*$idx}]]
	    lappend Command(cache) "$res" "[lrange $args 1 end]"
	    ${log}::debug "Returning cached result of ´$args´: $res"
	    return $res
	}
	incr idx
    }

    # Otherwise, call command and store in cache.
    if { [catch {uplevel $args} res] == 0 } {
	if { [llength $Command(cache)] > [expr {2*$Command(-maxstore)}] } {
	    ${log}::debug "Max number of parameters $Command(-maxstore)\
                           for $Command(cmd) reached"
	    while { [llength $Command(cache)] \
			> [expr {2*$Command(-maxstore)}] } {
		set Command(cache) [lrange $Command(cache) 2 end]
	    }
	}
	lappend Command(cache) "$res" "[lrange $args 1 end]"
	${log}::debug "Cached result of ´$args´ as $res"
    } else {
	global errorInfo
	global errorCode

	set eInfo $errorInfo
	set eCode $errorCode
	error $res $eInfo $eCode
    }

    return $res
}


# ::rescacher::config -- Configure command result cacher.
#
#	This command set or get the options of a command result cache.
#
# Arguments:
#	cmd	Main command (e.g. name of procedure)
#	args	list of options (-timeout -retries -redirects)
#
# Results:
#	Return all options, the option requested or set the options
#
# Side Effects:
#	None.
proc ::rescacher::config { cmd args } {
    variable RC
    variable log

    upvar \#0 [__cmd_store $cmd] Command

    set o [lsort [array names Command "-*"]]

    if { [llength $args] == 0 } {      ;# Return all results
	set result ""
	foreach name $o {
	    lappend result $name $Command($name)
	}
	return $result
    }

    foreach {opt value} $args {        ;# Get onr or set some
	if { [lsearch $o $opt] == -1 } {
	    return -code error "Unknown option $opt, must be: [join $o ", " ]"
	}
	if { [llength $args] == 1 } {  ;# Get one config value
	    return $Command($opt)
	}
	set Command($opt) $value         ;# Set the config value
    }
}


# ::rescacher::defaults -- Set/Get defaults for all new result cacher
#
#	This command sets or gets the defaults opetions for all new
#	command result cacher, it will not perpetrate on existing
#       commands, use ::rescacher::config instead.
#
# Arguments:
#	args	List of -key value or just -key to get value
#
# Results:
#	Return all options, the option requested or set the options
#
# Side Effects:
#	None.
proc ::rescacher::defaults { args } {
    variable RC
    variable log

    set o [lsort [array names RC "-*"]]

    if { [llength $args] == 0 } {      ;# Return all results
	set result ""
	foreach name $o {
	    lappend result $name $RC($name)
	}
	return $result
    }

    foreach {opt value} $args {        ;# Get onr or set some
	if { [lsearch $o $opt] == -1 } {
	    return -code error "Unknown option $opt, must be: [join $o ,]"
	}
	if { [llength $args] == 1 } {  ;# Get one config value
	    return $RC($opt)
	}
	set RC($opt) $value           ;# Set the config value
    }
}
