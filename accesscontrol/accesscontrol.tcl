# accesscontrol.tcl --
#
#	This module a generic access control language loosely modelled
#	after the host access control which is found in most UNIX
#	systems.  The access control language is built around two
#	files with the extensions .allow (or .alw) and .deny (or
#	.dny).  These files contain a number of lines, each matching a
#	single access rule, where rule specifications are separated by
#	":".  The first item in the line is the name of a resource,
#	while the second item is a list of names of accessing entities
#	(IP address, host names, etc.).  These list are separated by
#	space characters or comas.  These items (or part of the list)
#	are string patterns (string match like) that will be matched
#	when checking for access allowance.  The special keywords ALL
#	are understood as * and are there for compatibility reasons.
#	Traditional allow/deny file specification uses two files.
#	This module also supports the extended syntax where lines can
#	have a third (: separated) directive which can be ALLOW or
#	DENY and let users keep all directives in a single more
#	manageable file.
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

package require Tcl 8.2
package require logger
package require diskutil

package provide accesscontrol 1.0

namespace eval ::accesscontrol {
    # Variables of name ::accesscontrol::Control_<id> and
    # are created as arrays to support access control context

    # Initialise global state
    variable AC
    if {![info exists AC]} {
	array set AC {
	    controls   ""
	    id_gene    0
	    loglevel   warn
	}
	variable log [::logger::init accesscontrol]
	${log}::setlevel $AC(loglevel)
    }

    namespace export new allow loglevel
}


# ::accesscontrol::loglevel -- Set/Get current log level.
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
proc ::accesscontrol::loglevel { { loglvl "" } } {
    variable AC
    variable log

    if { $loglvl != "" } {
	if { [catch "${log}::setlevel $loglvl"] == 0 } {
	    set AC(loglevel) $loglvl
	}
    }

    return $AC(loglevel)
}


# ::accesscontrol::__find_access_control -- Finds an access control
#
#	Finds an access control through its allowance file.
#
# Arguments:
#	alw_fn	Full path to the allow file name
#
# Results:
#	Return the identifier of the access control or an empty string.
#
# Side Effects:
#	None.
proc ::accesscontrol::__find_access_control { alw_fn } {
    variable AC
    variable log

    foreach ac $AC(controls) {
	upvar \#0 $ac Control

	if { $Control(allow_fname) eq $alw_fn } {
	    return $ac
	}
    }

    return ""
}


# ::accesscontrol::__find_file -- Finds a file
#
#	Finds if a file can be found with one of the extensions passed
#	as a parameter.
#
# Arguments:
#	basenm	Full path to file, with or without the extensions.
#	exts	List of possible extensions for the file.
#
# Results:
#	Return full path to the file if it readable, otherwise an empty string
#
# Side Effects:
#	None.
proc ::accesscontrol::__find_file { basenm exts } {
    variable AC
    variable log

    # Check if the basename does not already contain one of the
    # extensions, in which case we return the name at once
    set fext [file extension $basenm]
    foreach ext $exts {
	if { [string index $ext 0] != "." } {
	    set ext ".$ext"
	}
	if { [string equal -nocase $ext $fext] } {
	    if { [file readable $basenm] } {
		${log}::debug "$basenm matches one of the extensions $exts"
		return $basenm
	    } else {
		${log}::warn \
		    "$basenm has one extension from $exts, but does not exist"
		return ""
	    }
	}
    }

    # No matching extension in the basename try to append these and
    # look for a file.
    foreach ext { .allow .alw } {
	if { [string index $ext 0] != "." } {
	    set ext ".$ext"
	}
	set fname ${basenm}${ext}
	if { [file readable $fname] } {
	    ${log}::debug "$basenm completed with extension $ext"
	    return $fname
	}
    }

    ${log}::info "Could not find $basenm with extension $exts"
    return ""
}


# ::accesscontrol::__check -- Check an access against a control file
#
#	This command will check an access for a resource and return a
#	keyword expressing allowance, denial or ignorance.  The
#	default specification is used when parsing files no allow/deny
#	directive as keywords at the end of lines (and thus the
#	specification comes implicitely from the name of the file).
#
# Arguments:
#	fname	Name of file to parse.
#	access	Incoming accessing identifier.
#	resource	Name of resource being controlled.
#	dft	Default directive (ALLOW or DENY)
#
# Results:
#	Return ALLOW, DENY, UNKNOWN or ERROR
#
# Side Effects:
#	None.
proc ::accesscontrol::__check { fname access resource dft } {
    variable log
    variable AC

    ${log}::debug "Opening $fname for access check against $resource: $dft?"
    if { [catch {open $fname} fd] == 0 } {
	while { ! [eof $fd] } {
	    set line [string trim [gets $fd]]
	    
	    if { $line ne "" } {
		set firstchar [string index $line 0]
		if { [string first $firstchar "\#!;"] < 0 } {
		    set atoms [split $line ":"]
		    set resource_spec [string trim [lindex $atoms 0]]
		    if { [string equal -nocase $resource_spec ALL] } {
			set resource_spec "*"
		    }
		    set access_spec [string trim [lindex $atoms 1]]
		    if { [string equal -nocase $access_spec ALL] } {
			set access_spec "*"
		    }
		    set allowance [string toupper \
				       [string trim [lindex $atoms 2]]]
		    if { $allowance eq "" } {
			set allowance $dft
		    } elseif { [lsearch "ALLOW DENY" $allowance] < 0 } {
			${log}::warn "In $fname, $allowance unknown directive,\
                                      defaulting to $dft"
		    }
		    if { [string match -nocase $resource_spec $resource] } {
			set match 0
			foreach acs [split $access_spec " ,\t"] {
			    if { [string match -nocase $acs $access] } {
				close $fd
				return $allowance
			    }
			}
		    }
		}
	    }
	}
	close $fd
	return "UNKNOWN"
    } else {
	${log}::warn "Could not open $fname for access check"
	return "ERROR"
    }
}


# ::accesscontrol::allow -- Check access for a resource
#
#	This command will check the access of an incoming accessing
#	entity (name of host, BT address, whatever) to a given
#	resource (defaulting to the name of the program).
#
# Arguments:
#	ctl	Identifier of access controller.
#	access	Incoming access entity identifier.
#	resource	Name of resource being accessed.
#
# Results:
#	Return ALLOW, DENY, UNKNOWN or ERROR
#
# Side Effects:
#	None.
proc ::accesscontrol::allow { ctl access { resource "" } } {
    variable AC
    variable log
    global argv0
    
    if { [lsearch $AC(controls) $ctl] < 0 } {
	${log}::warn "$ctl is not an existing access controller"
	return 0;    # Default to a restrictive behaviour
    }

    if { $resource eq "" } {
	if { $argv0 eq "" } {
	    set resource [file tail [info script]]
	} else {
	    set resource [file tail $argv0]
	}
    }

    upvar \#0 $ctl Control

    # Check allowance against the allowance file. If we explicitely
    # get to know yes or no, return, otherwise simply go on with
    # denial check.
    if { $Control(allow_fname) ne "" } {
	set res [__check $Control(allow_fname) $access $resource "ALLOW"]
	switch $res {
	    ERROR -
	    DENY {
		return 0
	    }
	    ALLOW {
		return 1
	    }
	    UNKNOWN {
		# Do nothing here on purpose, just go on with denial
	    }
	}
    }

    if { $Control(deny_fname) ne "" } {
	set denial [__check $Control(deny_fname) $access $resource "DENY"]
	switch { $denial } {
	    ERROR -
	    DENY {
		return 0
	    }
	    UNKNOWN -
	    ALLOW {
		return 1
	    }
	}
    } else {
	return 1
    }
}


# ::accesscontrol::new -- Creates a new access control context
#
#	This command will create a new access control context and
#	return a refernce to this context that should be used in all
#	further calls.  An empty basename for the access control files
#	willl be understood as a way to allow all accesses to all
#	resources.  The basename can either point to a file with an
#	.allow or .alw extension (in which case the corresponding
#	.deny or .dny file will be used) or to the basname for these
#	two files.  The files will be read for each access control check
#
# Arguments:
#	basenm	Base name for access control directives.
#
# Results:
#	Return the identifier of the access control.
#
# Side Effects:
#	None.
proc ::accesscontrol::new { { basenm "" } } {
    variable AC
    variable log

    set allow_fname ""
    set deny_fname ""
    if { $basenm ne "" } {
	set basenm [::diskutil::fname_resolv $basenm]
	if { [file readable $basenm] } {
	    set allow_fname $basenm
	    set deny_fname ""
	} else {
	    set allow_fname [__find_file $basenm [list .allow .alw]]
	    if { $allow_fname ne "" } {
		set deny_fname [__find_file [file rootname $allow_fname] \
				    [list .deny .dny]]
	    } else {
		set deny_fname ""
	    }
	}
    }

    set ctl [__find_access_control $allow_fname]
    if { $ctl eq "" } {
	set ctl "::accesscontrol::Control_$AC(id_gene)"
	incr AC(id_gene)

	upvar \#0 $ctl Control
	set Control(allow_fname) $allow_fname
	set Control(deny_fname) $deny_fname
	set Control(id) $ctl

	lappend AC(controls) $ctl
    }
    return $ctl
}
