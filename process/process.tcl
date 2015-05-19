# cmdserver.tcl --
#
#	This module provides a programmatic interface to work with
#	processes that have typically been started from outside
#	Tcl/Tk.
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

package require Tcl 8.2
package require logger
package require diskutil

namespace eval ::process {
    # Initialise global state
    variable PS
    global tcl_platform

    if {![info exists PS]} {
	array set PS {
	    psdir      ""
	    loglevel   warn
	    winexeext  "exe com bat"
	    winimpls   "pslist pv"
	}
	variable log [::logger::init [string trimleft [namespace current] ::]]
	variable libdir [file dirname \
			     [::diskutil::absolute_path [info script]]]
	${log}::setlevel $PS(loglevel)
    }

    namespace export loglevel find list full_list kill
}


# ::process::loglevel -- Set/Get current log level.
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
proc ::process::loglevel { { loglvl "" } } {
    variable PS
    variable log

    if { $loglvl != "" } {
	if { [catch "${log}::setlevel $loglvl"] == 0 } {
	    set PS(loglevel) $loglvl
	}
    }

    return $PS(loglevel)
}



# ::process::__binary --
#
#	Returns the full path to a ps utility executable, empty string
#	otherwise.
#
# Arguments:
#	exe	Executable to look for
#
# Results:
#	Empty string on error, otherwise full path.
#
# Side Effects:
#	None.
proc ::process::__binary { { exe "ps" } } {
    variable PS
    variable log
    global tcl_platform
    
    if { $tcl_platform(platform) == "windows" } {
	if { [file extension $exe] == "" } {
	    set exelist ""
	    foreach ext $PS(winexeext) {
		lappend exelist ${exe}.${ext}
	    }
	} else {
	    set exelist $exe
	}
    } else {
	set exelist $exe
    }

    set ps_exe ""
    foreach fname $exelist {
	set fullpath [file join $PS(psdir) $fname]
	if { [file executable $fullpath] } {
	    set ps_exe $fullpath
	    break
	}
    }

    if { $ps_exe == "" } {
	${log}::warn "Could not find $exe for execution"
    }
    
    return $ps_exe
}


# ::process::find --
#
#	Find one or several running processes whose name match a given
#	regular expression.
#
# Arguments:
#	rgx	Regular expression to match (defaults to all processes)
#
# Results:
#	Return a list (possibly empty) of process identifiers.
#
# Side Effects:
#	None.
proc ::process::find { { rgx "" } } {
    variable PS
    variable log
    global tcl_platform

    ${log}::info "Getting process list"

    set ps_exe [::process::__binary "$PS(ps)"]
    if { $ps_exe == "" } {
	${log}::error "Cannot find $PS(ps)"
	return {}
    }
    set ps_exe "\"$ps_exe\" $PS(find_args)"

    ${log}::debug "Executing '$ps_exe'... "
    if { [catch {open "|$ps_exe"} fd] == 0 } {
	set pids ""
	while { ! [eof $fd] } {
	    set line [gets $fd]
	    # First isolate the second word of the line, which should
	    # contain the process identifier, whichever platform we
	    # are on.
	    set idx 0
	    set wdcount 0
	    while { $wdcount < 1 } {
		set idx [tcl_startOfNextWord $line $idx]
		incr wdcount
	    }
	    set pid [string trim \
			 [string range $line $idx [tcl_endOfWord $line $idx]]]

	    # Now get rid of non compliant process identifiers (both
	    # platform specific commands write a header at the
	    # beginning of the table.
	    if { [regexp "\\d+" $pid] } {
		# On windows, the name of the applications is the
		# first word.  On UNIX we have seen to have the
		# command at the 4th word and onwards.
		if { $tcl_platform(platform) == "windows" } {
		    set appname [string trim \
				     [string range $line 0 [expr $idx - 1]]]
		} else {
		    set idx 0
		    set wdcount 0
		    while { $wdcount < 3 } {
			set idx [tcl_startOfNextWord $line $idx]
			incr wdcount
		    }
		    set appname [string range $line $idx end]
		}

		# Now, we have an application name, match it against
		# the regular expression and return.  This will work
		# best on UNIX since there we have all the arguments
		# after the name of the application itself.
		if { [regexp $rgx $appname] } {
		    lappend pids $pid
		}
	    }
	}
	catch {close $fd}
	return $pids
    } else {
	${log}::warn "Error when executing $ps_exe: $fd"
	return {}
    }
}


# ::process::list --
#
#	List all currently running processes.
#
# Arguments:
#	None.
#
# Results:
#	Return a list (possibly empty) of all running process identifiers.
#
# Side Effects:
#	None.
proc ::process::list { } {
    variable PS
    variable log
    global tcl_platform

    ${log}::info "Getting process list"

    set ps_exe [::process::__binary "$PS(ps)"]
    if { $ps_exe == "" } {
	${log}::error "Cannot find $PS(ps)"
	return {}
    }
    set ps_exe "\"$ps_exe\" $PS(list_args)"

    ${log}::debug "Executing '$ps_exe'... "
    if { [catch {open "|$ps_exe"} fd] == 0 } {
	set pids ""
	while { ! [eof $fd] } {
	    set line [gets $fd]
	    if { [llength $line] >= 2 \
		     && [regexp "^\[0-9\]+$" [lindex $line 1]] } {
		lappend pids [lindex $line 1]
	    }
	}
	catch {close $fd}
	return $pids
    } else {
	${log}::warn "Error when executing $ps_exe: $fd"
	return {}
    }
}



# ::process::full_list --
#
#	Lists all currently running system processes.  The list is a
#	list of list with the following content (in that order and
#	whenever possible, otherwise an empty string): process id,
#	user, program, ppid, vmsize
#
# Arguments:
#	None.
#
# Results:
#	Return a list (possibly empty) with information for all
#	running processes.
#
# Side Effects:
#	None.
proc ::process::full_list { } {
    variable PS
    variable log
    global tcl_platform

    ${log}::info "Getting process list"

    set ps_exe [::process::__binary "$PS(ps)"]
    if { $ps_exe == "" } {
	${log}::error "Cannot find $PS(ps)"
	return {}
    }
    set ps_exe "\"$ps_exe\" $PS(fulllist_args)"

    ${log}::debug "Executing '$ps_exe'... "
    if { [catch {open "|$ps_exe"} fd] == 0 } {
	set pids ""
	while { ! [eof $fd] } {
	    set line [gets $fd]
	    if { [regexp "^\[0-9\]+$" [lindex $line 1]] } {
		if { $tcl_platform(platform) == "windows" } {
		    set p_pid [lindex $line 1]
		    set p_user ""
		    set p_prg [lindex $line 0]
		    set p_ppid ""
		    if { $PS(ps) == "pv" } {
			set p_vmsize ""
		    } else {
			set p_vmsize [lindex $line 2]
		    }
		} else {
		    set p_pid [lindex $line 1]
		    set p_user [lindex $line 0]
		    set p_prg [lindex $line 4]
		    set p_ppid [lindex $line 2]
		    set p_vmsize [lindex $line 3]
		}
		lappend pids [::list $p_pid $p_user $p_prg $p_ppid $p_vmsize]
	    }
	}
	catch {close $fd}
	return $pids
    } else {
	${log}::warn "Error when executing $ps_exe: $fd"
	return {}
    }
}



# ::process::kill --
#
#	Kill a list of given processes.
#
# Arguments:
#	pids	List of processes to kill
#
# Results:
#	Return a list of processes for which the kill command successed.
#
# Side Effects:
#	Will actively attempt to kill processes that are external to
#	this process!
proc ::process::kill { pids } {
    variable PS
    variable log
    global tcl_platform

    ${log}::info "Killing processes $pids"

    set ps_exe [::process::__binary "$PS(kill)"]
    if { $ps_exe == "" } {
	${log}::error "Cannot find $PS(kill)"
	return {}
    }
    set ps_exe "\"$ps_exe\" $PS(kill_args)"

    set killed [::list]
    foreach pid $pids {
	${log}::debug "Killing process $pid"

	set res [catch "exec $ps_exe $pid" output]
	if { $res == 0 } {
	    lappend killed $pid
	} else {
	    if { $PS(kill_match) != "" } {
		set ptn [string map [::list "%p" "$pid"] $PS(kill_match)]
		if { ! [string match $ptn $output] } {
		    ${log}::warn "Error when executing $ps_exe: $output"
		} else {
		    lappend killed $pid
		}
	    }
	}
    }
    return $killed
}


# ::process::__init -- Initialise this module
#
#	This procedure is automatically called when the package is
#	required.  It will see to detect ps implementations and to
#	detect which one should be use.
#
# Arguments:
#	None.
#
# Results:
#	1 on success, 0 on failure
#
# Side Effects:
#	None.
proc ::process::__init { } {
    variable PS
    variable log
    variable libdir
    global tcl_platform

    if { $tcl_platform(platform) == "windows" } {
	set psroot [file join $libdir "bin" $tcl_platform(platform)]
	if { ! [file isdirectory $psroot] } {
	    ${log}::error "'$psroot' is not a directory or does not exist"
	} else {
	    foreach impl $PS(winimpls) {
		set impldir [file join $psroot $impl]
		if { [file isdirectory $impldir] } {
		    switch -- $impl {
			"pslist" {
			    set PS(psdir) $impldir
			    array set PS {
				ps            "pslist"
				kill          "pskill"
				fulllist_args " -accepteula -m"
				list_args     " -accepteula"
				kill_args     " -accepteula"
				kill_match    "*Process*%p*killed*"
				find_args     " -accepteula"
			    }
			}
			"pv" {
			    set PS(psdir) $impldir
			    array set PS {
				ps            "pv"
				kill          "pv"
				fulllist_args " -e"
				list_args     ""
				kill_args     " -kif"
				kill_match    "*(%p)*"
				find_args     ""
			    }
			}
		    }
		    break
		}
	    }
	    if { $PS(psdir) == "" } {
		${log}::warn "Could not find any known implementation!"
		array set PS {
		    psdir         ""
		    ps            "ps"
		    kill          "kill"
		    fulllist_args " -e -o user,pid,ppid,vsz,comm"
		    list_args     " -e -o user,pid,ppid,vsz"
		    kill_args     ""
		    kill_match    ""
		    find_args     " -e -o \"%u %p %P %a\""
		}
	    }
	}
    } else {
	# The following command ensures that we only ouput safe
	# arguments when doing the process list.  Formerly, I was
	# using -ef but that leads to the dumping of program
	# arguments, which itself can output curly braces and other
	# oddities that cannot be parsed by the code that follows.
	# set ps_exe "/bin/ps -e -o \"%u %p %P\""
	array set PS {
	    psdir         "/bin"
	    ps            "ps"
	    kill          "kill"
	    fulllist_args " -e -o user,pid,ppid,vsz,comm"
	    list_args     " -e -o user,pid,ppid,vsz"
	    kill_args     ""
	    find_args     " -e -o \"%u %p %P %a\""
	}
    }
}


namespace eval ::process {
    variable inited
    if { ! [info exists inited] } {
	set inited [__init]
    }
}

package provide process 1.1
