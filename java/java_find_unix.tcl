# java_find_unix.tcl --
#
#	This module contains the UNIX implementation of the
#	::java::find implementation.
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.



# ::java::jre_version --
#
#	Find whether the JVM placed at the location pointed at
#	within the arguments matches a given version number.
#
# Arguments:
#	j_root	Root directory to the JVM
#	vnumber_p	Variable that will contain the version number
#	vnum_match	Pattern that the version number should match.
#
# Results:
#	Return the full path to the JVM that matches the version
#	number pattern passed as argument, or an empty string if no
#	match.
#
# Side Effects:
#	Start the JVM to poll for its version number.
proc ::java::jre_version { j_root { vnumber_p "" } { vnum_match "*" } } {
    variable log

    if { $vnumber_p != "" } {
	upvar $vnumber_p vnumber
    }

    set j_exe [file join $j_root java]
    if { [file executable $j_exe] } {
	set j_exe [auto_execok $j_exe]
    } else {
	set j_exe ""
    }

    if { $j_exe != "" } {
	${log}::debug "Checking JVM at $j_exe"
	set success [catch "exec $j_exe -version" res]
	set vstart [string first "version" $res]
	if { $vstart >= 0 } {
	    set vstart [string first "\"" $res $vstart]
	    incr vstart
	    set vend [string first "\"" $res $vstart]
	    incr vend -1
	    set vnumber [string range $res $vstart $vend]
	}
	if { [string match $vnum_match $vnumber] } {
	    return $j_exe
	}
    }

    return ""
}



# ::java::find --
#
#	Find the location of a JVM that matches a given version number
#	pattern.  The algorithm uses a number of hints to find
#	possible JVMs locations.  First it looks in the current PATH,
#	then looks at the location of the JAVA_HOME environment
#	variable and finally at a number of more or less standard
#	locations.
#
# Arguments:
#	vnumber_p	Variable that will contain the actual version number
#	vnum_match	Pattern that the JVM version number should match.
#
# Results:
#	Return the full path to the JVM, empty if none were find.
#
# Side Effects:
#	This command will possibly start several JVMs until it finds
#	one that matches its arguments..
proc ::java::find { { vnumber_p "" } { vnum_match "*" } } {
    global env

    if { $vnumber_p != "" } {
	upvar $vnumber_p vnumber
    }

    set vnumber ""
    if { [array names env PATH] != "" } {
	foreach dir [split $env(PATH) ":"] {
	    set j_exe [::java::jre_version $dir vnumber $vnum_match]
	    if { $j_exe != "" } {
		return $j_exe
	    }
	}
    }

    if { [array names env JAVA_HOME] != "" } {
	set j_exe [::java::jre_version [file join $env(JAVA_HOME) bin] \
		       vnumber $vnum_match]
	if { $j_exe != "" } {
	    return $j_exe
	}
    }

    if { [file isdirectory "/usr/java"] } {
	set j_exe [::java::jre_version "/usr/java/bin" vnumber $vnum_match]
	if { $j_exe != "" } {
	    return $j_exe
	}

	foreach dir [glob "/usr/java/*"] {
	    set j_exe [::java::jre_version [file join $dir bin] vnumber \
			   $vnum_match]
	    if { $j_exe != "" } {
		return $j_exe
	    }
	}
    }
    return ""
}
