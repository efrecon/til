# java_find_unix.tcl --
#
#	This module contains the UNIX implementation of the
#	::java::find implementation.
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

package require registry




# ::java::jre_get --
#
#	Look whether the JVM pointed at the registry home passed as a
#	parameter matches a given version number pattern and return in
#	that case the home location of the root of the JVM
#
# Arguments:
#	jre_root	Root of the JVM in the registry.
#	vnumber_p	Variable that will contain the actual version number
#	vnum_match	Pattern that the version number should match.
#
# Results:
#	Return the full Windows path to the JVM executable, or an
#	empty string.
#
# Side Effects:
#	None.
proc ::java::__jre_get { jre_root { vnumber_p "" } { vnum_match "*" } } {
    set java_home ""

    if { $vnumber_p != "" } {
	upvar $vnumber_p vnumber
    }

    set res [catch [list registry get "$jre_root" CurrentVersion] vnumber]
    if { $res == 0 && [string match $vnum_match $vnumber] } {
	catch [list registry get "${jre_root}\\${vnumber}" JavaHome] java_home
    } else {
	set res [catch [list registry keys "$jre_root"] allkeys]
	if { $res == 0 } {
	    foreach vnumber $allkeys {
		if { [string match $vnum_match $vnumber] } {
		    set res [catch [list registry get \
					"${jre_root}\\${vnumber}" \
					JavaHome] java_home]
		    if { $res == 0 } {
			break
		    }
		}
	    }
	}
    }

    return $java_home
}



# ::java::__jre_exec --
#
#	Look whether the JVM pointed at the registry home passed as a
#	parameter matches a given version number pattern and return in
#	that case the location of the JVM executable in a Tcl-friendly
#	format.
#
# Arguments:
#	jre_root	Root of the JVM in the registry.
#	vnumber_p	Variable that will contain the actual version number
#	vnum_match	Pattern that the version number should match.
#
# Results:
#	Return the full tcl-compatible path to the JVM executable, or an
#	empty string.
#
# Side Effects:
#	None.
proc ::java::__jre_exec { jre_root { vnumber_p "" } { vnum_match "*" } } {
    if { $vnumber_p != "" } {
	upvar $vnumber_p vnumber
    }

    set j_home [::java::__jre_get $jre_root vnumber $vnum_match]
    if { $j_home != "" } {
	# Make a Tcl-friendly path of the horrible windows backslashes
	set j_home [regsub -all "\\\\" $j_home "/"]
	set j_exe [file join $j_home "bin" "java.exe"]
	if { [file executable $j_exe] } {
	    return $j_exe
	}
    }

    return ""
}


# ::java::find --
#
#	Find the location of a JVM that matches a given version number
#	pattern.  The algorithm uses a number of hints to find
#	possible JVMs locations.  It looks for both JVMs by Sun and by
#	IBM and handles both JRE and JDK versions.
#
# Arguments:
#	vnumber_p	Variable that will contain the actual version number
#	vnum_match	Pattern that the JVM version number should match.
#
# Results:
#	Return the full path to the JVM executable, empty if none were find.
#
# Side Effects:
#	None.
proc ::java::find { { vnumber_p "" } { vnum_match "*" } } {
    if { $vnumber_p != "" } {
	upvar $vnumber_p vnumber
    }

    set vnumber ""
    set root "HKEY_LOCAL_MACHINE\\Software\\JavaSoft"
    set j_exe [::java::__jre_exec "$root\\Java Runtime Environment" vnumber \
		   $vnum_match]
    if { $j_exe == "" } {
	set j_exe [::java::__jre_exec "$root\\Java Development Kit" vnumber \
		       $vnum_match]
	if { $j_exe == "" } {
	    set root "HKEY_LOCAL_MACHINE\\Software\\IBM"
	    set j_exe [::java::__jre_exec "$root\\Java Runtime Environment" \
			   vnumber $vnum_match]
	    if { $j_exe == "" } {
		set j_exe [::java::__jre_exec "$root\\Java Development Kit" \
			       vnumber $vnum_match]
	    }
	}
    }
    
    return $j_exe
}
