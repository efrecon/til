# hostalive.tcl --
#
#	This modules interfaces the PING command line tool to check
#	the liveness of hosts.
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.



package require Tcl 8.2
package require logger

package provide hostalive 1.0

namespace eval ::hostalive {
    # Initialise the global state
    variable HA
    if {![::info exists HA]} {
	array set HA {
	    idgene        0
	    loglevel      "warn"
	    hosts         ""
	    pingexe       ""
	    headermax     5
	    -period       5
	    -alivecb      ""
	    inheritance   "-period -alivecb"
	}
	variable log [::logger::init [string trimleft [namespace current] ::]]
	${log}::setlevel $HA(loglevel)
    }

    namespace export loglevel new get head syncget synchead infile cancel
}


# ::hostalive::loglevel -- Set/Get current log level.
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
proc ::hostalive::loglevel { { loglvl "" } } {
    variable HA
    variable log

    if { $loglvl != "" } {
	if { [catch "${log}::setlevel $loglvl"] == 0 } {
	    set HA(loglevel) $loglvl
	}
    }

    return $HA(loglevel)
}


# ::hostalive::__platform -- Return platform unique identification
#
#       This command forms a unique string for the platform on which
#       the current interpreter is running.  The string contains not
#       only the operating system, but also the CPU architecture.
#       This implementation originates from critcl (I think).
#       Examples of returned strings are Windows-x86, Linux-x86 or
#       Darwin-ppc
#
# Arguments:
#	none
#
# Results:
#	Returns a unique platform identification string.
#
# Side Effects:
#	None.
proc ::hostalive::__platform {} {
    global tcl_platform

    set plat [lindex $tcl_platform(os) 0]
    set mach $tcl_platform(machine)
    switch -glob -- $mach {
	sun4* { set mach sparc }
	intel -
	i*86* { set mach x86 }
	"Power Macintosh" { set mach ppc }
    }
    switch -- $plat {
	AIX   { set mach ppc }
	HP-UX { set mach hppa }
    }
    return "$plat-$mach"
}


# ::hostalive::__pingclose -- Close pipe
#
#	This command stops the monitoring of a host through closing
#	the file descriptor of the last ping command that was started.
#
# Arguments:
#	id	Identifier of the watch
#
# Results:
#	none
#
# Side Effects:
#	Will close the pipe file descriptor in some cases.
proc ::hostalive::__pingstop { id } {
    variable HA
    variable log

    set idx [lsearch $HA(hosts) $id]
    if { $idx >= 0 } {
	set varname "::hostalive::Monitor_${id}"
	upvar \#0 $varname Monitor

	${log}::debug "Closing ping pipe for $id ($Monitor(fullname))"
	fileevent $Monitor(fd) readable ""
	catch {close $Monitor(fd)}
	set Monitor(fd) ""
    }
}


# ::hostalive::__pingread -- Reads ping lines.
#
#	This command reads ping lines.  It closes the file descriptor
#	if the EOF was reached (PING died!), or if we were in
#	autoclose mode, which means that the ping implementation does
#	not support the ability to set the period or to continuously
#	ping a host.
#
# Arguments:
#	id	Identifier of the watch
#
# Results:
#	none
#
# Side Effects:
#	Will close the pipe file descriptor in some cases.
proc ::hostalive::__pingread { id } {
    variable HA
    variable log

    set idx [lsearch $HA(hosts) $id]
    if { $idx >= 0 } {
	set varname "::hostalive::Monitor_${id}"
	upvar \#0 $varname Monitor

	# EOF reached or blocked. PING has probably died, so restart the whole
	# continuous ping processing again.
	if { [fblocked $Monitor(fd)] || [eof $Monitor(fd)] } {
	    ${log}::warn "EOF reached or blocked, ping probably died,\
                          restarting"
	    __pingstop $id
	    after [expr {$Monitor(-period) * 1000}] \
		::hostalive::__pingstart $id
	    return
	}

	# Otherwise, there is perhaps something to read.  If there is
	# (non-empty), analyse what is being said there and conclude
	# if the host is alive or not.
	set line [gets $Monitor(fd)]
	if { $line ne "" } {
	    ${log}::debug "Analysing: $line"

	    # Guess state of host
	    set oldstate $Monitor(state)
	    if { [regexp "time(=|<|>)" $line] } {
		set Monitor(state) on
	    } else {
		set Monitor(state) off
	    }
	    
	    # Tell the callers
	    if { $oldstate ne $Monitor(state) && $Monitor(-alivecb) ne "" } {
		if { [catch {eval $Monitor(-alivecb) $id $Monitor(hostname) \
				 $Monitor(state) $Monitor(fullname) \
				 $Monitor(ip)} err] } {
		    ${log}::warn "Error when calling liveness callback: $err"
		}
	    }
	    ${log}::info "$Monitor(fullname) at $Monitor(ip): $Monitor(state)"
	    if { [string is true $Monitor(autoclose)] } {
		__pingstop $id
		after [expr {$Monitor(-period) * 1000}] \
		    ::hostalive::__pingstart $id
	    }
	}
    }
}


# ::hostalive::__pingstart -- Start pinging a host
#
#	This command starts to watch for the presence of a remote
#	host.  It parses the header got back from PING, stores the
#	fullname (unalised) of the host, its IP number and sees to
#	parse the answers from PING.
#
# Arguments:
#	id	Identifier of the watch
#
# Results:
#	?
#
# Side Effects:
#	Will start pinging the remote host.
proc ::hostalive::__pingstart { id } {
    variable HA
    variable log

    set idx [lsearch $HA(hosts) $id]
    if { $idx >= 0 } {
	set varname "::hostalive::Monitor_${id}"
	upvar \#0 $varname Monitor

	# Build the command.  Be sure to understand the pingalways and
	# pingperiod options if we can, since these might help us not
	# restarting the ping program over and over.
	set Monitor(autoclose) on
	set cmd "$HA(pingexe)"
	if { $HA(pingperiod_opt) ne "" } {
	    append cmd " [format $HA(pingperiod_opt) $Monitor(-period)]"
	    if { $HA(pingalways_opt) ne "" } {
		append cmd " $HA(pingalways_opt)"
		set Monitor(autoclose) off
	    }
	}
	if { [string is true $Monitor(autoclose)] } {
	    if { $HA(pingcount_opt) ne "" } {
		append cmd " [format $HA(pingcount_opt) 1]"
	    }
	}
	append cmd " $Monitor(hostname)"

	if { [catch {open "|$cmd"} fd] } {
	    ${log}::warn "Could not start ping as '$cmd'!"
	} else {
	    ${log}::debug "Started ping with: '$cmd'"
	    set Monitor(fd) $fd
	    fconfigure $fd -buffering line

	    # Read header
	    set nbread 0
	    while { $nbread < $HA(headermax) } {
		set line [gets $fd]
		incr nbread
		if { $line ne "" } {
		    ${log}::debug "Header line: $line"
		    # The following algorithm is able to handle the
		    # following types of lines, which are ping results

		    # on a number of architectures:
		    # Pinging doping.sics.se [193.10.65.131] with 32 bytes of data:
		    # PING doping.sics.se (193.10.65.131): 56 data bytes
		    # PING jenny.sics.se (193.10.64.99) 56(84) bytes of data.

		    # First try to look for the word PING (no matter the case)
		    set idx [string first "PING" [string toupper $line]]
		    if { $idx >= 0 } {
			# Once we have found PING, advance to the
			# beginning of the next word, which should
			# contain the full unaliased name of the host
			set idx [string first " " $line $idx]
			incr idx
			set end [string first " " $line $idx]
			set fullname \
			    [string trim [string range $line $idx $end]]
			if { $fullname != $Monitor(fullname) } {
			    set Monitor(fullname) $fullname
			}
			if { [regexp {\d+\.\d+\.\d+\.\d+} $fullname] } {
			    set Monitor(ip) $fullname
			} else {
			    set Monitor(ip) "?.?.?.?"
			}

			# The IP number seem to be enclosed in
			# repeating characters.  The algorithm below
			# does more than what I have seen, it tries to
			# be smart..
			foreach enclosure { "()" "\[\]" "<>" "\"\"" } {
			    set open \
				[string first [string index $enclosure 0] \
				     $line $end]
			    if { $open >= 0 } { break }
			}
			if { $open >= 0 } {
			    set close \
				[string first [string index $enclosure 1] \
				     $line $open]
			    incr open
			    incr close -1
			    set ip \
				[string trim [string range $line $open $close]]
			    if { $ip != $Monitor(ip) } {
				set Monitor(ip) $ip
			    }
			}

			# Get off the header analysis loop, we are
			# ready to get on analysing the ping lines.
			break
		    }
		}
	    }
	    
	    # Now start reading what PING really says...
	    fconfigure $fd -blocking off -buffering line
	    fileevent $fd readable [list ::hostalive::__pingread $id]
	}
    } else {
	${log}::warn "$id does not identify a known host monitoring"
    }
}


# ::hostalive::new -- Start a new continuous ping
#
#	This command starts to watch for the presence of a remote host.
#
# Arguments:
#	host	Name or IP of host
#	args	list of options (-period -alivecb)
#
# Results:
#	Return the identifier of the hostalive continuous check or a negative
#       number on error.
#
# Side Effects:
#	Will start pinging the remote host.
proc ::hostalive::new { hostname args } {
    variable HA
    variable log
    global env

    if { $HA(pingexe) eq "" } {
	# Decide default values. Work on most unix systems.
	switch [__platform] {
	    Linux-x86 {
		set HA(pingalways_opt) "-n"
		set HA(pingcount_opt) "-c %d"
		set HA(pingperiod_opt) "-i %d"
		set pingexe "/bin/ping"
	    }
	    Windows-x86 {
		set HA(pingalways_opt) "-t"
		set HA(pingcount_opt) "-n %d"
		set HA(pingperiod_opt) ""
		set pingexe \
		    [file join [file dirname $env(COMSPEC)] ping.exe]
	    }
	    default {
		set HA(pingalways_opt) "-n"
		set HA(pingcount_opt) "-c %d"
		set HA(pingperiod_opt) "-i %d"
		set pingexe "ping"
	    }
	}
	set HA(pingexe) [auto_execok $pingexe]
	if { $HA(pingexe) eq "" } {
	    ${log}::warn "Could not expand $pingexe to an executable"
	    return -1
	}
    }

    set id [incr HA(idgene)]
    set varname "::hostalive::Monitor_${id}"
    upvar \#0 $varname Monitor

    set Monitor(hostname) $hostname
    set Monitor(fullname) ""
    set Monitor(ip) ""
    set Monitor(fd) ""
    set Monitor(state) ""
    foreach opt $HA(inheritance) {
	set Monitor($opt) $HA($opt)
    }
    lappend HA(hosts) $id
    eval config $id $args

    __pingstart $id

    return $id
}


# ::hostalive::config -- Configure a get operation
#
#	This command set or get the options of a get operation.
#
# Arguments:
#	cid	Connection identifier
#	args	list of options (-timeout -retries -redirects)
#
# Results:
#	Return all options, the option requested or set the options
#
# Side Effects:
#	None.
proc ::hostalive::config { id args } {
    variable HA
    variable log

    # Check that this is one of our hosts
    set idx [lsearch $HA(hosts) $id]
    if { $idx < 0 } {
	${log}::warn "Mass get URL identifier $id is not valid"
	return -code error "Identifier invalid"
    }

    set varname "::hostalive::Monitor_${id}"
    upvar \#0 $varname Monitor

    set o [lsort [array names Monitor "-*"]]

    if { [llength $args] == 0 } {      ;# Return all results
	set result ""
	foreach name $o {
	    lappend result $name $Monitor($name)
	}
	return $result
    }

    foreach {opt value} $args {        ;# Get onr or set some
	if { [lsearch $o $opt] == -1 } {
	    return -code error "Unknown option $opt, must be: [join $o ", " ]"
	}
	if { [llength $args] == 1 } {  ;# Get one config value
	    return $Monitor($opt)
	}
	set Monitor($opt) $value         ;# Set the config value
    }
}


# ::hostalive::defaults -- Set/Get defaults for all new connections
#
#	This command sets or gets the defaults opetions for all new
#	connections, it will not perpetrate on existing pending
#	connections, use ::hostalive::config instead.
#
# Arguments:
#	args	List of -key value or just -key to get value
#
# Results:
#	Return all options, the option requested or set the options
#
# Side Effects:
#	None.
proc ::hostalive::defaults { args } {
    variable HA
    variable log

    set o [lsort [array names HA "-*"]]

    if { [llength $args] == 0 } {      ;# Return all results
	set result ""
	foreach name $o {
	    lappend result $name $HA($name)
	}
	return $result
    }

    foreach {opt value} $args {        ;# Get onr or set some
	if { [lsearch $o $opt] == -1 } {
	    return -code error "Unknown option $opt, must be: [join $o ,]"
	}
	if { [llength $args] == 1 } {  ;# Get one config value
	    return $HA($opt)
	}
	set HA($opt) $value           ;# Set the config value
    }
}
