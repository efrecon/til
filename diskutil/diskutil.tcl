# diskutil.tcl --
#
#	A number of disk utilities, oriented around temporary files.
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

# Version History:
#   1.0 - First version, developed for WebPR & WebPath
#   1.1 - Improved version for ACCORD
#   1.2 - Adding features for the deskpoll facility.
#   1.3 - Adding path expansion
#   1.4 - Adding file concatenation routing
#   1.5 - Name shift for the package and added name resolution routine.
#   1.6 - Adding normalisation emulation.
#   1.7 - Adding file rotation

package require Tcl 8.2
package require logger

package provide diskutil 1.9

namespace eval ::diskutil {
    
    # Initialise global state.
    variable DiskUtil
    if {![info exists DiskUtil]} {
	array set DiskUtil {
	    tmpdir             ""
	    gtmpdir            ""
	    winemu_dir         "~/.CommonStore"
	    loglevel           warn
	    comments           "\#"
	    empty              {\"\" \{\} -}
	    dft_prgpath        ""
	    resolvers          {::tcl_platform "" 0 * ::env "" 1 *}
	    idgene             0
	}
	variable log [::logger::init diskutil]
	${log}::setlevel $DiskUtil(loglevel)
    }

    # Export commands
    namespace export double_backslah absolute_path append_path
    namespace export platform_tmp temporary_filename temporary_file
    namespace export temporary_directory clean_directory
    namespace export concat_files fname_resolv normalize rotate
    namespace export fname_resolve resolver
}


# ::diskutil::loglevel -- Set/Get current log level.
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
proc ::diskutil::loglevel { { loglvl "" } } {
    variable DiskUtil
    variable log

    if { $loglvl != "" } {
	if { [catch "${log}::setlevel $loglvl"] == 0 } {
	    set DiskUtil(loglevel) $loglvl
	}
    }

    return $DiskUtil(loglevel)
}


# ::diskutil::normalize -- Normalize filename
#
#	This performs a job that is similar to file normalize and can
#	be used for Tcl implementations that do not support this new
#	command. (taken and adapted from wiki: http://wiki.tcl.tk/1540)
#
# Arguments:
#	fname	Input filename
#	keeprel	Do not resolve relative path, keep them relative
#
# Results:
#	Return a file name where all .. and . have been replaced.
#
# Side Effects:
#	None.
proc ::diskutil::normalize { fname { keeprel 0 } } {
    variable DiskUtil
    variable log
    
    set sp [file split $fname]
    if {[file pathtype [lindex $sp 0]] == "relative" && ! $keeprel} {
	set sp [file split [eval [list file join [pwd]] $sp]]
    }
    set np {}
    foreach ele $sp {
	if {$ele != ".."} {
	    if {$ele != "."} { lappend np $ele }
	} else {
	    if { $keeprel } {
		set np [lrange $np 0 [expr {[llength $np] - 2}]]
	    } elseif {[llength $np]> 1} {
		set np [lrange $np 0 [expr {[llength $np] - 2}]]
	    }
	}
    }
    if {[llength $np] > 0} { return [eval file join $np] }
    return ""
}


# ::diskutil::double_backslash --
#
#	Double every backslash in a native filename
#
# Arguments:
#	fname_in	Input filename
#
# Results:
#	Return a file name where all backslashes have been doubled and
#	all remaining characters are unchanged.
#
# Side Effects:
#	None.
proc ::diskutil::double_backslash { fname_in } {
    set len [string length $fname_in]
    set fname_out ""
    for { set i 0 } { $i<$len } { incr i } {
	set char [string index $fname_in $i]
	if { $char == "\\" } {
	    append fname_out "\\\\"
	} else {
	    append fname_out $char
	}
    }

    return $fname_out
}



# ::diskutil::absolute_path --
#
#	Resolve a path name so that it is absolute from the root of
#	the file system.
#
# Arguments:
#	relpath	Relative file or dir path
#
# Results:
#	Returns the absolute path for a file, i.e. no . or .. in path.
#	Return "" in case of impossible resolution.
#
# Side Effects:
#	This implementation actually changes to the directory of the
#	specified path and back to the initial directory.  If
#	something goes wrong while changing directory and the command
#	abruptbly returns, the current directory might be different
#	than before calling the command.
proc ::diskutil::absolute_path { relpath } {
    variable log

    set abspath ""
    set olddir [pwd]

    if { [file isdirectory $relpath] } {
	if { [catch "cd $relpath"] != 0 } {
	    ${log}::error "Cannot change to directory $relpath"
	    return ""
	}
	set abspath [pwd]
    } else {
	set dirname [file dirname $relpath]
	set filename [file tail $relpath]
	if { [catch {cd "$dirname"} err] } {
	    ${log}::error "Cannot change to directory $dirname: $err"
	    return ""
	}
	set abspath [file join [pwd] $filename]
    }
    cd $olddir

    ${log}::debug "$relpath resolved to $abspath"
    return $abspath
}


# ::diskutil::append_path --
#
#	This command arranges to append directories to a PATH like
#	variable.  It aims at offering a platform-independent
#	interface to this kind of environment variable.
#
# Arguments:
#	path_p	"pointer" to path variable.
#	dirs	List of directories to append
#	native	Should directories be translated to native file path format?
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::diskutil::append_path { path_p dirs { native 0 } } {
    global tcl_platform

    upvar $path_p path

    # Make sure the path variable exists and initialise it if it does not.
    if { [catch "set path"] != 0 } {
	set path ""
    }
    
    foreach dir $dirs {
	if { $dir != "" } {
	    # Convert to native file path format if requested
	    if { $native } {
		set d [file nativename $dir]
	    } else {
		set d $dir
	    }
	    
	    # Add the directory to the path variable.
	    if { $path == "" } {
		set path $d
	    } else {
		if { $tcl_platform(platform) == "windows" } {
		    append path ";$d"
		} else {
		    append path ":$d"
		}
	    }
	}
    }

    return ""
}


# ::diskutil::expand_filename --
#
#	Search a path like variable for a directory containing a given
#	file and return its path. This command is platform aware and
#	handles path separators as ";" on Windows and as ":" on UNIX.
#	Return an empty string if nothing was found.
#
# Arguments:
#	path	; or : separated path
#	fname	Name of file to look for
#
# Results:
#	Returns the full access path to the first file matching the
#	name and pointed at by the path specification.  An empty
#	string is returned if the file was not found.
#
# Side Effects:
#	None.
proc ::diskutil::expand_filename { path fname } {
    global tcl_platform

    if { $tcl_platform(platform) == "windows" } {
	set alldirs [split $path ";"]
    } else {
	set alldirs [split $path ":"]
    }
    
    foreach d $alldirs {
	set fullpath [file join $d $fname]
	if { [file exists $fullpath] } {
	    return $fullpath
	}
    }

    return ""
}


# ::diskutil::expand_execname --
#
#	Search a path like variable for a directory containing a given
#	file and return its path. This command is platform aware and
#	handles path separators as ";" on Windows and as ":" on UNIX.
#	On Windows, the command is intelligent enough to look for BAT
#	files and exe files also (appended to the raw file name).
#
# Arguments:
#	path	; or : separated path
#	fname	Name of file to look for
#
# Results:
#	Returns the full access path to the executable file matching
#	the name and pointed at by the path specification.  An empty
#	string is returned if the file was not found.
#
# Side Effects:
#	None.
proc ::diskutil::expand_execname { path fname } {
    global tcl_platform

    if { $tcl_platform(platform) == "windows" } {
	set alldirs [split $path ";"]
	set postfix [list "" ".bat" ".exe"]
    } else {
	set alldirs [split $path ":"]
	set postfix [list ""]
    }
    
    foreach d $alldirs {
	foreach p $postfix {
	    set fullpath [file join $d ${fname}${p}]
	    if { [file executable $fullpath] } {
		return $fullpath
	    }
	}
    }

    return ""
}


proc ::diskutil::__first_dir_of { dirlist } {
}


# ::diskutil::platform_tmp --
#
#	Returns the location of a valid machine-wide
#	platform-dependent temporary directory where files can be
#	stored.  The implementation is aware of the current
#	environment and of the current platform.  First it looks at
#	some well-known environement variables that might point to
#	valid directories.  Second it looks for a temporary directory
#	in well-known locations and depending on the platform.  On
#	Windows, this implementation attempts to prioritise the file
#	space that is associated to the current user.  The procedure
#	caches its result for future access.
#
# Arguments:
#	None.
#
# Results:
#	A valid temporary directory for file storage, an empty string
#	on errors.
#
# Side Effects:
#	None.
proc ::diskutil::platform_tmp { } {
    global tcl_platform env
    variable DiskUtil
    variable log

    if { $DiskUtil(tmpdir) == "" } {
	set dirlist [list]
	if { [array names env "TEMP"] == "TEMP" } {
	    lappend dirlist $env(TEMP)
	}
	if { [array names env "TMP"] == "TMP" } {
	    lappend dirlist $env(TMP)
	}
	if { [array names env "TMPDIR"] == "TMPDIR" } {
	    lappend dirlist $env(TMPDIR)
	}
	if { $tcl_platform(platform) == "windows" } {
	    # Windows Vista introduced a new way to organise things
	    # up.  Start with LOCALAPPDATA, it is completely safe
	    # since on older OSes the variable doesn't even
	    # exist. Usually, the TEMP variable should be the same as
	    # the test below, but in some extreme cases TEMP is not
	    # set (for example, when starting from within cygwin).
	    if { [array names env "LOCALAPPDATA"] == "LOCALAPPDATA" } {
		lappend dirlist [file join $env(LOCALAPPDATA) "Temp"]
	    }
	    # This is on NT, 98, etc. Problem is that "Local Settings"
	    # actually sometimes is in the local language of the OS,
	    # e.g. "Lokala Inställningar" in Swedish.
	    if { [array names env "USERPROFILE"] == "USERPROFILE" } {
		lappend dirlist \
		    [file join $env(USERPROFILE) "Local Settings" "Temp"]
	    }
	    # There used to be such a dir, I wonder it still exists at
	    # all and is accessible. Adding it for "in case".
	    if { [array names env "WINDIR"] == "WINDIR" } {
		lappend dirlist [file join $env(WINDIR) "Temp"]
	    }
	    if { [array names env "SYSTEMROOT"] == "SYSTEMROOT" } {
		lappend dirlist [file join $env(SYSTEMROOT) "Temp"]
	    }
	} else {
	    lappend dirlist "/usr/tmp/"
	    lappend dirlist "/tmp"
	}

	# Parse the list of possible temporary directories and try to
	# see if they really are directories that we can access.
	# Maybe we should also do a file writable, but on windows this
	# always returns 1 on directories (it seems to be at least).
	foreach d $dirlist {
	    if { [file isdirectory $d] } {
		# The glob will catch if the directory does not
		# properly exist or cannot be access.
		if { ! [catch {glob -nocomplain -directory $d -- *} err] } {
		    set DiskUtil(tmpdir) $d
		    break
		}
	    }
	}
	if { $DiskUtil(tmpdir) == "" } {
	    ${log}::error "Could not find a temporary directory!"
	} else {
	    ${log}::notice "Will use $DiskUtil(tmpdir) as temporary directory"
	}
    }
    return [::diskutil::double_backslash $DiskUtil(tmpdir)]
}



# ::diskutil::temporary_filename --
#
#	This command returns an adequate name to be used for a
#	temporary file.  It supports both a prefix and an extension so
#	as to ease future recognition of orphane temporary files.
#
# Arguments:
#	pfx	Optional prefix to the name of the file
#	ext	ptional extension of the file (might or not start with
#               a dot, a dot will always separate the name from the
#               extension anyhow)
#
# Results:
#	Returns a file name ready for use.
#
# Side Effects:
#	None.
proc ::diskutil::temporary_filename { { pfx "" } { ext "" } } {
    set time [clock clicks]
    if { $time < 0 } {
	set time [ expr - $time]
    }

    set name $pfx
    if { $pfx != "" } {
	append name "_"
    }

    append name [format "%d_%d" $time [expr int(1000*rand())]]
    if { $ext != "" } {
	if { [string index $ext 0] == "." } {
	    append name "$ext"
	} else {
	    append name ".$ext"
	}
    }

    return $name
}


# ::diskutil::temporary_file --
#
#	This command returns the absolute path to a temporary file.
#	It is up to the caller to create the file and check whether
#	creation was successful or not.  This command is a utility
#	wrapper around platform_tmp and temporary_filename.
#
# Arguments:
#	pfx	Optional prefix to the name of the file
#	ext	ptional extension of the file (might or not start with
#               a dot, a dot will always separate the name from the
#               extension anyhow)
#
# Results:
#	An absolute path to a temporary filename.
#
# Side Effects:
#	None.
proc ::diskutil::temporary_file { { pfx "" } { ext "" } } {
    # Return the path
    return [file join [platform_tmp] [temporary_filename $pfx $ext]]
}


# ::diskutil::temporary_file --
#
#	This command creates and returns the absolute path to a
#	temporary directory.  This command will create directories in
#	the directory returned by platform_tmp.
#
# Arguments:
#	pfx	Optional prefix to push in from of dir name
#
# Results:
#	An absolute path to a temporary directory.
#
# Side Effects:
#	None.
proc ::diskutil::temporary_directory { { prefix "" } } {
    set dir [platform_tmp]

    set done 0
    while { ! $done } {
	set time [clock clicks]
	if { $time < 0 } {
	    set time [ expr - $time]
	}
	set rawname [format "%d~%d" [expr $time % 100] [expr int(100*rand())]]
	if { $prefix == "" } {
	    set name $rawname
	} else {
	    set name "${prefix}_${rawname}"
	}
	set dirname [file join $dir $name]

	set res [catch "file mkdir $dirname"]
	if { $res == 0 && [file exists $dirname] } {
	    set done 1
	}
    }

    return $dirname
}


# ::diskutil::clean_directory --
#
#	This command cleans up all files and directories that match a
#	given pattern in a directory.  It allows to specify specific
#	files or directory that should be kept.  If <rm_ptn> is empty,
#	all files and sub directories of the directory are suppressed.
#	The command return the number of items removed, a negative
#	number on error.
#
# Arguments:
#	d	Directory to clean.
#	rm_ptn	Patterns (string match like) of file and directory names
#               to remove
#	keep_ptn	Patterns (string match like) of file and directory
#                       namesto keep (allows to keep some of the files and
#                       directories that would be removed by <rm_ptn>
#
# Results:
#	The number of file removed, a negative number in case of error.
#
# Side Effects:
#	Effectively remove the files!
proc ::diskutil::clean_directory { d { rm_ptn {} } { keep_ptn {} } } {
    variable log

    set current_d [pwd]
    
    ${log}::notice "Cleanup directory $d"

    if { [catch "cd \"$d\""] != 0 } {
	${log}::error "Could not change directory to \"$d\"!"
	return -1
    }
    
    set nb_removed 0
    if { [llength $rm_ptn] == 0 } {
	if { [catch "glob *" files] == 0 } {
	    foreach f $files {
		${log}::info "Removing: $f" 4
		if { [catch "file delete -force -- $f"] == 0 } {
		    incr nb_removed
		}
	    }
	}
    } else {
	foreach ptn $rm_ptn {
	    if { [catch "glob \"$ptn\"" files] == 0 } {
		foreach f $files {
		    set match 0
		    foreach ptn $keep_ptn {
			if { [string match "$ptn" "$f"] } {
			    set match 1
			    break
			}
		    }
		    
		    if { ! $match } {
			${log}::info "Removing: $f"
			if { [catch "file delete -force -- $f"] == 0 } {
			    incr nb_removed
			}
		    }
		}
	    }
	}
    }

    cd $current_d
    return $nb_removed
}


proc ::diskutil::global_platform_tmp {} {
    global tcl_platform env
    variable DiskUtil
    variable log

    if { $DiskUtil(gtmpdir) == "" } {
	set dirlist {}
	if { $tcl_platform(platform) == "windows" } {
	    if { [info exists env(ALLUSERSPROFILE)] } {
		# Cope with XP, where we had an Application Data, but
		# also with later versions av windows, where we don't.
		if { [info exists env(APPDATA)] } {
		    lappend dirlist \
			[file join $env(ALLUSERSPROFILE) \
			     [file tail $env(APPDATA)]]
		}
		lappend dirlist \
		    [file join $env(ALLUSERSPROFILE) "Application Data"]
		lappend dirlist \
		    [file join $env(ALLUSERSPROFILE)]
	    }
	    # There used to be such a dir, I wonder it still exists at
	    # all and is accessible. Adding it for "in case".
	    if { [info exists env(WINDIR)] } {
		lappend dirlist [file join $env(WINDIR) "Temp"]
	    }
	    if { [info exists env(SYSTEMROOT)] } {
		lappend dirlist [file join $env(SYSTEMROOT) "Temp"]
	    }
	} else {
	    set dirlist [list "/usr/tmp" "/var/tmp"]
	    if { [info exists env(TMPDIR)] } {
		lappend dirlist [file join $env(TMPDIR)]
	    }
	}

	# Parse the list of possible temporary directories and try to
	# see if they really are directories that we can access.
	# Maybe we should also do a file writable, but on windows this
	# always returns 1 on directories (it seems to be at least).
	foreach d $dirlist {
	    if { [file isdirectory $d] } {
		# The glob will catch if the directory does not
		# properly exist or cannot be access.
		if { ! [catch {glob -nocomplain -directory $d -- *} err] } {
		    set DiskUtil(gtmpdir) $d
		    break
		}
	    }
	}
	if { $DiskUtil(gtmpdir) == "" } {
	    ${log}::error "Could not find a temporary directory!"
	} else {
	    ${log}::notice "Will use $DiskUtil(gtmpdir) as global \
                            temporary directory"
	}
    }
    return [::diskutil::double_backslash $DiskUtil(gtmpdir)]
}


proc ::diskutil::global_app_directory { { prgpath "" } { prgname "" } } {
    variable DiskUtil
    variable log

    if { $prgpath == "" } {
	# Make sure to actually store the program path in a local
	# variable in case the path contained a "." and was therefore
	# related to the current directory at start. This because,
	# obviously, the current directory can change at any time!
	if { $DiskUtil(dft_prgpath) == "" } {
	    if { [info exists ::starkit::topdir] } {
		set DiskUtil(dft_prgpath) [normalize $::starkit::topdir]
	    } else {
		set DiskUtil(dft_prgpath) [normalize $::argv0]
	    }
	    ${log}::info "Default program path is at $DiskUtil(dft_prgpath)"
	}
	set prgpath $DiskUtil(dft_prgpath)
    }

    if { $prgname eq "" } {
	set prgname [file rootname [file tail $prgpath]]
    }
    set dir [file join [global_platform_tmp] $prgname]
    if { [catch {file mkdir $dir} err] } {
	${log}::error "Cannot create global storage for app at $dir: $err"
	set dir ""
    }
    return $dir
}


# ::diskutil::concat_files --
#
#	Concatenate one or serveral files one after the other to
#	produce another file.  
#
# Arguments:
#	dst_file	Name of destination file
#	in_files	List of paths to the files to be concatenated.
#
# Results:
#	Return the number of concatenated files.
#
# Side Effects:
#	Create a file.  If it existed, its old content will be lost.
proc ::diskutil::concat_files { dst_file in_files } {
    set nb_f 0
    set f [open $dst_file "w"]
    foreach in_file $in_files {
	if { [file exists $in_file] && [file readable $in_file] } {
	    set in [open $in_file]
	    puts $f [read $in]
	    close $in
	    incr nb_f
	}
    }
    close $f

    return $nb_f
}


# ::diskutil::__mapper -- Sugared array content mapper
#
#       This procedure takes the content of an array and arranges for all the
#       instances of the names of the array (surrounded by a marker sign) to be
#       replaced by their value in the incoming string. The procedure is also
#       able to add a prefix to all indices for replacement, and to filter only
#       a subset of the array.
#
# Arguments:
#	str	String in which to perform replacements
#	array_p	Name of array with which to perform replacements
#	pfx	String to prefix to all indices before they are replaced
#	nocase	1 for case insensiteness detection of indice-based keys.
#	filter	Filter for array indices, only matching indices to be considered
#	marker	Marker sign to surround the indice sub-strings to be detected
#		and replaced.
#
# Results:
#       The string where all occurences of the indices of the array, prefixed
#       and surrounded by the marker have been replaced by the value of the
#       indice.
#
# Side Effects:
#       None.
proc ::diskutil::__mapper { str array_p {pfx ""} {nocase 0} {filter *} {marker %} } {
    set mapper [list]
    upvar $array_p ARY
    foreach i [array names ARY $filter] {
	lappend mapper ${marker}${pfx}${i}${marker} $ARY($i)
    }
    if { $nocase } {
	return [string map -nocase $mapper $str]
    } else {
	return [string map $mapper $str]
    }
    return "";  # Never reached
}


# ::diskutil::fname_resolv --
#
#	Replace %key% strings in a series of filenames and return the
#	resulting file name.  The recognised keys are the
#	following. Every index of the tcl_platform global variable
#	will be recognised as a key, this includes useful keys such as
#	%os%, %platform% or %user%. A number of handcrafted keys are
#	also recognised.  These are %progdir% - the directory path to
#	the "executable" being run, i.e. the main Tcl script - and
#	%progname% - the raw name of the executable being run, without
#	extension, nor directory name.  An argument allows to use
#	another alternative path for %progname% and %progdir%, but
#       these keys will default to using the global argv0 variable. It is
#       possible to register additional resolvers for the keys being recognised,
#       see resolver for details.
#
# Arguments:
#	fnames	(list of) file names.
#	prgpath	Full path to program being used for %progdir% and %progname%
#	dynamic	List of (dynamic) key and their values to also recognise
#
# Results:
#	A modified (list of) file names
#
# Side Effects:
#	None.
proc ::diskutil::fname_resolv { fnames { dynamic {} } { prgpath "" } } {
    global tcl_platform env
    variable DiskUtil
    variable log
    
    # Swap prgpath and dynamic if dynamic isn't a list which number of
    # arguments can be divided by two, this is to ensure compatibility
    # with older uses of this interface.
    set ldyn [llength $dynamic]
    if { $ldyn > 0 && [expr {$ldyn%2}] == 1 } {
	foreach {prgpath dynamic} [list $dynamic $prgpath] break
    }

    # Start by insert dynamic and user keys, do this at ONCE to make
    # sure they have precedence.
    array set DYN $dynamic
    set fnames [__mapper $fnames DYN]
    
    # Then go on with all known resolvers, this includes support for ::env and
    # ::tcl_platform, as these are setup by default
    foreach {resolver pfx nocase filter} $DiskUtil(resolvers) {
	upvar \#0 $resolver ARY;   # Access array at toplevel
	set fnames [__mapper $fnames ARY $pfx $nocase $filter]
    }
    
    # Now construct a final BUILTIN array that contains indices to various other
    # good things to have when trying to resolve path names...
    
    # Provides support for "%hostname%"
    if { [info commands "::dnsresolv::hostname"] != "" } {
	set BUILTIN(hostname) [::dnsresolv::hostname]
    } else {
	set BUILTIN(hostname) [info hostname]
    }

    # Provides support for "%progdir%", when no specific program path
    # is specified, guess it intelligently from the arguments, taking
    # properly care of starkit:ed programs.
    if { $prgpath == "" } {
	# Make sure to actually store the program path in a local
	# variable in case the path contained a "." and was therefore
	# related to the current directory at start. This because,
	# obviously, the current directory can change at any time!
	if { $DiskUtil(dft_prgpath) == "" } {
	    if { [info exists ::starkit::topdir] } {
		set DiskUtil(dft_prgpath) [normalize $::starkit::topdir]
	    } else {
		set DiskUtil(dft_prgpath) [normalize $::argv0]
	    }
	    ${log}::info "Default program path is at $DiskUtil(dft_prgpath)"
	}
	set prgpath $DiskUtil(dft_prgpath)
    }
    set BUILTIN(progdir) [file dirname $prgpath]
    set BUILTIN(progname) [file rootname [file tail $prgpath]]

    # Emulates some well-known environment variables when not on windows.
    if { $tcl_platform(platform) ne "windows" } {
	set BUILTIN(APPDATA) [file join $DiskUtil(winemu_dir) AppData]
	set BUILTIN(LOCALAPPDATA) [file join $DiskUtil(winemu_dir) Local
    }

    return [__mapper $fnames BUILTIN]
}


# ::diskutil::fname_resolve -- Same as fname_resolv
#
#       Wrapper to call fname_resolv and fix spelling
#
# Arguments:
#	args	Same as fname_resolv
#
# Results:
#       See fname_resolv
#
# Side Effects:
#       None.
proc ::diskutil::fname_resolve {args} {
    return [uplevel 1 [linsert $args 0 [namespace current]::fname_resolv]
}


# ::diskutil::resolver -- Add resolver for fname_resolv
#
#       This procedure can be used to globally modify the behaviour of
#       fname_resolv(e) and to add a set of (dynamic) keys/values to be
#       recognised when performing substitutions in the file names. The
#       implementation recognises two types of additional resolver: the dynamic
#       content of a globally accessible array, or an even-long (static) list of
#       keys and their values.
#
# Arguments:
#	r	Resolver, name of global array or even-long list
#	type	ARRAY or LIST
#	pfx	String to prefix to all keys associated to this resolver when
#		substitution occurs.
#	nocase	Set to true to perform case insensitve substitution for resolver
#	filter	Glob-style filter to match against the keys to consider for
# 		substitution.
#
# Results:
#       None.
#
# Side Effects:
#       Will modify the future behaviour of all calls to fname_resolv.
proc ::diskutil::resolver { r {type "ARRAY"} {pfx ""} {nocase off} {filter *} } {
    variable DiskUtil
    variable log

    set type [string toupper $type]
    switch -- $type {
	"ARRAY" {
	    lappend DiskUtil(resolvers) \
		    $r $pfx [string is true $nocase] $filter	    
	}
	"LIST" {
	    set vname [namespace current]::__resolver_storage_[incr DiskUtil(idgene)]
	    array set $vname $r
	    lappend DiskUtil(resolvers) \
		    $vname $pfx [string is true $nocase] $filter	    
	}
	default {
	    ${log}::warn "$type is unknown!"			
	}
    }
}


# ::diskutil::equal_files -- Test file equality
#
#       Tests if files are equal
#
# Arguments:
#       fname1	Path to first file
#       fname2	Path to second file
#       binary	Are file binary files.
#
# Results:
#       1 if files are identical, 0 otherwise
#
# Side Effects:
#       Reads the content of both file in memory, so this is not
#       effective on big files.
proc ::diskutil::equal_files { fname1 fname2 { binary off } } {
    variable log

    # Choose opening mode and select a "mode" that will be used for
    # log messages.
    set opening "r"
    set mode "textually"
    if { [string is true $binary] } {
	set mode "binary"
	append opening "b"
    }

    # Open both files and place their content in ndta and odta (n for
    # new, o for old).
    set nfd [open $fname1 $opening]
    set ndta [read $nfd]
    close $nfd

    set ofd [open $fname2 $opening]
    set odta [read $ofd]
    close $ofd

    # Test equality and return result, outputing some log if necessary.
    set res [string equal $ndta $odta]
    if { $res } {
	${log}::debug "$fname1 and $fname2 were $mode equal"
    } else {
	${log}::info "$fname1 and $fname2 were $mode different"
    }
    return $res
}


# ::diskutil::__rotate_fname -- Generate rotation name
#
#	Generates the name of a rotation file.  If the input file name
#	contains a zero (0) or one (1), it will be replaced by the
#	number passed as an argument. Otherwise, the number will be
#	appended to the file name.
#
# Arguments:
#	rot_fname	Name template of file to rotate
#	num     	Sequencer
#
# Results:
#	Return the rotation file name
#
# Side Effects:
#	None.
proc ::diskutil::__rotate_fname { rot_fname num } {
    # Find first figure from the list in the main template
    foreach figure [list 0 1] {
	set idx [string last $figure $rot_fname]
	if { $idx >= 0 } {
	    break
	}
    }

    if { $idx >= 0 } {
	set out_fname [string range $rot_fname 0 [expr {$idx - 1}]]
	append out_fname $num
	append out_fname [string range $rot_fname [expr {$idx + 1}] end]
    } else {
	set out_fname "${rot_fname}.${num}"
    }
    return $out_fname
}


# ::diskutil::rotate -- Rotate files in directory
#
#	Rotate files (such as log files) in a directory.  An input
#	file (generally the file that is dynamically changed) will be
#	moved in the archive and a number of existing files in the
#	archive will be kept.
#
# Arguments:
#	in_fname	Full path to the input file, the routine handles
#                       archive increments correctly even if it contains a
#                       zero.
#	keep     	Number of archive files to keep (zero is valid!)
#	rot_fname     	Path to the rotation archive.  Any 0 or 1 in the name
#                       will be replaced by the archive increments (otherwise
#                       these will be appended).  Relative paths will be
#                       appended to the directory of the input file.  An
#                       empty string takes the input file name as a template.
#
# Results:
#	None.
#
# Side Effects:
#	Will rename, move and even remove files on disk appropriately
proc ::diskutil::rotate { in_fname { keep 4 } { rot_fname "" } } {
    variable DiskUtil
    variable log

    # Decide upon the name template for the rotation files.  An empty
    # string will be understood as the same as the "pumped" file,
    # i.e. the incoming file for the rotation.  Otherwise, the
    # rotation files are joined with the directory of the input file,
    # which allows to cover both the case of relative and absolute
    # path names for the destination.
    if { $rot_fname eq "" } {
	set rot_fname $in_fname
    } else {
	set rot_fname [file join [file dirname $in_fname] $rot_fname]
    }

    # If the input (pump) file name contains a zero then rotation will
    # start at index 1 only.
    if { [string last "0" [file tail $in_fname]] } {
	set start 1
    } else {
	set start 0
    }

    # Rotate already existing files.
    for { set i [expr {$keep - 1}]} { $i >= $start } { incr i -1 } {
	if { [file exists [__rotate_fname $rot_fname $i]] } {
	    ${log}::debug "Moving \"[__rotate_fname $rot_fname $i]\"\
		           to \"[__rotate_fname $rot_fname [expr {$i + 1}]]\""
	    if { [catch {file rename -force -- \
			     [__rotate_fname $rot_fname $i] \
			     [__rotate_fname $rot_fname [expr {$i + 1}]]} \
		      err] } {
		${log}::warn \
		    "Could not rename \"[__rotate_fname $rot_fname $i]\"\
		     to \"[__rotate_fname $rot_fname [expr {$i + 1}]]\": $err"
	    }
	}
    }

    # And perform the last rotation, which means installing the input
    # pump file as first in the rotation list.
    if { $keep >= 1 } {
	${log}::debug \
	    "Moving \"$in_fname\" to \"[__rotate_fname $rot_fname $start]\""
	if { [catch {file rename -force -- \
			 $in_fname [__rotate_fname $rot_fname $start]} err] } {
	    ${log}::warn "Could not rename \"$in_fname\"\
                          to \"[__rotate_fname $rot_fname $start]\": $err"
	}
    } else {
	${log}::debug "Removing \"$in_fname\""
	if { [catch {file delete -force -- $in_fname} err] } {
	    ${log}::warn "Could not remove \"in_fname\": $err"
	}
    }
}


# ::diskutil::__computesignature -- Compute a file signature
#
#	This command computes an integer signature for the file which
#	name is passed as an argument.  The signature can be based on
#	the root name of the file, its size and its modification,
#	which should guarantee that the integer will change as soon as
#	the file changes.
#
# Arguments:
#	fname	Path to file
#	what	What to compute
#	max	max value
#
# Results:
#	An integer identifying the file
#
# Side Effects:
#	None.
proc ::diskutil::__computesignature { fname {what "size mtime name"}
				      {max 2147483647}} {
    variable DiskUtil
    variable log

    set sig 0
    if { [lsearch $what "name"] >= 0 } {
	set rname [file tail $fname]
	for { set i 0 } { $i < [string length $rname] } { incr i } {
	    set sig [expr ($sig + [scan [string index $rname $i] %c]) % $max]
	}
    }
    if { [lsearch $what "size"] >= 0 } {
	set size [file size $fname]
	set sig [expr ($sig + [file size $fname]) % $max]
    }
    if { [lsearch $what "mtime"] >= 0 } {
	set mtime [file mtime $fname]
	set sig [expr ($sig + [file mtime $fname]) % $max]
    }
    ${log}::debug "Signature of $fname is $sig"

    return $sig
}


# ::diskutil::match -- Match a file name
#
#	Match a file name against a list of allowed / denied filters
#	and return whether the file should be treated or not.
#
# Arguments:
#	fname	name of file
#	consider	List of regular expressions for file names to consider
#	ignore	List of regular expressions for file names to ignore
#
# Results:
#	Return a boolean telling whether the file should be handled or not.
#
# Side Effects:
#	None.
proc ::diskutil::match { fname {consider {".*"}} {ignore {}} } {
    variable DiskUtil
    variable log

    set do 0
    foreach rxp $consider {
	if { [catch {regexp $rxp $fname} res] == 0 } {
	    if { $res } {
		set do 1
		break
	    }
	} else {
	    ${log}::warn "Failed matching $rxp against $fname: $res"
	}
    }
    
    set dont 0
    foreach rxp $ignore {
	if { [catch {regexp $rxp $fname} res] == 0 } {
	    if { $res } {
		set dont 1
		break
	    }
	} else {
	    ${log}::warn "Failed matching $rxp against $fname: $res"
	}
    }

    return [expr {$do && !$dont}]
}


# ::diskutil::__signature -- Recursive signature computation
#
#	This command performs signature computation of directory trees
#	according to the directives contained in the context pointed
#	at by the identifier.  This command is the core of the
#	signature computation and performs the work as described in
#	::diskutil::signature.
#
# Arguments:
#	id	Identifier of signature context
#	fname	Name of file to test during recursion.
#
# Results:
#	None
#
# Side Effects:
#	Will store signature computation in the context.
proc ::diskutil::__signature { id fname } {
    variable DiskUtil
    variable log

    set varname ::diskutil::sig_$id
    upvar \#0 $varname Signature
    
    set fsig 0
    if { [match $fname $Signature(-consider) $Signature(-ignore)] } {
	if { [file isdirectory $fname] } {
	    if { ! ([string is true $Signature(-ignoretop)] \
			&& $fname == $Signature(fname)) } {
		set fsig [__computesignature $fname $Signature(-compute) \
			      $Signature(-max)]
	    }
	    set allfiles [glob -nocomplain ${fname}/*]
	    foreach f $allfiles {
		__signature $id $f
	    }
	} else {
	    set fsig [__computesignature $fname $Signature(-compute) \
			  $Signature(-max)]
	}
    }
    
    set Signature(signature) \
	[expr ($Signature(signature) + $fsig) % $Signature(-max)]
}


# ::diskutil::signature -- Compute a file/directory signature
#
#	This command computes an integer signature for the
#	file/directory which name is passed as an argument.  The
#	signature is based on the root name of the file, its size and
#	its modification, which should guarantee that the integer will
#	change as soon as the file changes.  Directories are recursed
#	across all their files to take inner changes into account.
#	This command takes options, all starting with a leading dash
#	(-) and followed by a value.  These are: -compute tells which
#	elements should be used for computing every single signature,
#	it is a list of the following strings: size (size of the
#	file), name (name (tail) of file), mtime (modification time)
#	and defaults to {size name mtime}; -ignoretop tells if the top
#	(directory) should be set aside when computing the signature
#	(which allows to compare directory hierarchies in different
#	places); -ignore allows to set aside special files or
#	directories (it is a list of regular expressions and defaults
#	to .*~$ .*bak$); -consider tells which files and directories
#	should be considered during traversal (same as above, defaults
#	to .*); -max is the maximum integer when doing signature
#	calculations, all elements will be added modulo that value.
#
# Arguments:
#	fname	Path to file
#
# Results:
#	An integer identifying the file
#
# Side Effects:
#	None.
proc ::diskutil::signature { fname args } {
    variable DiskUtil
    variable log

    # Create context with default values
    if { [array names DiskUtil idgene] == "" } { set DiskUtil(idgene) 0 }
    set id [incr DiskUtil(idgene)]
    set varname ::diskutil::sig_$id
    upvar \#0 $varname Signature
    array set Signature {
	-compute      "size name mtime"
	-ignoretop    off
	-consider     ".*"
	-ignore       ".*~$ .*bak$"
	-max          2147483647
    }
    set Signature(id) $id
    set Signature(fname) $fname
    set Signature(signature) 0
    
    # Parse options and store requested options.
    set o [lsort [array names Signature "-*"]]
    foreach {opt value} $args {
	if { [lsearch $o $opt] == -1 } {
	    return -code error "Unkown option $opt, must be: [join $o ", " ]"
	}
	set Signature($opt) $value
    }

    # Recurse through tree and do the work.
    __signature $id $fname
    
    # And return result.
    ${log}::info "Signature of $fname is $Signature(signature)"
    set sig $Signature(signature)
    unset Signature

    return $sig
}


# ::diskutil::lread -- Read lists from file
#
#       This is a generic "list reading" procedure that will read the
#       content of files where each line represents one element of a
#       list.  The procedure will gracefully ignore comments and empty
#       lines, thus providing a raw mechanism for reading
#       configurations files in a number of cases.  The procedure is
#       also able to count and control the number of elements in the
#       list that is read, forcing them to be a multiplier of xx and
#       cutting away the last elements not following the rule if
#       necessary.  This makes it perfect for parsing the result of
#       file reading using a foreach command.
#
# Arguments:
#	fname	Path to file to read
#	divider	Multiplier for number of elements, negative or zero to turn off
#	type	Type of file being read, used for logging output only.
#
# Results:
#       Return the elements contained in the file as a list.  If the
#       number of elements in the list had to be a multiplier of xx,
#       ending elements that do not follow the rule (if any) are
#       removed.  The list is empty on errors (or when no elements
#       were contained in the file.
#
# Side Effects:
#       None.
proc ::diskutil::lread { fname { divider -1 } { type "file" } } {
    variable DiskUtil
    variable log
    
    set vals [list]
    ${log}::info "Reading $type from $fname"
    if { [catch {open $fname} fd] } {
	${log}::error "Could not read $type from $fname: $fd"
    } else {
	while { ! [eof $fd] } {
	    set line [string trim [gets $fd]]
	    if { $line ne "" } {
		set firstchar [string index $line 0]
		if { [string first $firstchar $DiskUtil(comments)] < 0 } {
		    # Allow to add empty values
		    if { [lsearch -exact $DiskUtil(empty) $line] >= 0 } {
			lappend vals ""
		    } else {
			lappend vals $line
		    }
		}
	    }
	}
	close $fd

	set len [llength $vals]
	if { $divider > 0 } {
	    if { [expr {$len % $divider}] != 0 } {
		set keep [expr {($len / $divider)*$divider}]
		${log}::warn "$type $fname contained $len elements,\
                              wrong numer! Keeping $keep first ones"
		set vals [lrange $vals 0 [expr {$keep - 1}]]
	    } else {
		${log}::debug "Read $len elements from $type $fname"
	    }
	} else {
	    ${log}::debug "Read $len elements from $type $fname"
	}
    }

    return $vals
}


proc ::diskutil::hread { fd_or_fname args } {
    variable DiskUtil
    variable log

    # Segragates between file descriptors or file path, arrange for fd
    # to be a file descriptor where to read from, fail early when
    # opening the file.
    if { [catch {fconfigure $fd_or_fname} err] } {
	${log}::debug "We believe $fd_or_fname is a file path"
	set fd [open $fd_or_fname]
    } else {
	set fd $fd_or_fname
    }

    # Set default options and do some pretty basic option parsing.
    array set OPTS {
	-comments      "\#"
	-separators    ":"
	-trim          on
	-header        ""
	-command       {}
    }
    foreach {k v} $args {
	if { ![info exists OPTS($k)] } {
	    return -code error \
		"$k is an unknown option, should be one of\
                 [join [array names OPTS] {, }]"
	}
	set OPTS($k) $v
    }

    array set HDR {};  # Will contain the headers, if necessary and requested
    set content {};    # Will contain a list of the lines composing the content.

    # Initial state is either "reading the HEADERs", either jumping
    # straight into "reading the CONTENT".
    set state "HEADER"
    if { $OPTS(-separators) eq "" } {
	set state "CONTENT"
    }

    # Read the file line by line and segregate between header and content
    set unget ""
    while {![eof $fd]} {
	if { $unget ne "" } {
	    set line $unget
	    set unget ""
	} else {
	    set line [gets $fd]
	}
	set tline [string trim $line]
	
	switch -- $state {
	    "HEADER" {
		# Detect if first character of trimmed line is a
		# comment character, in which case we'll skip the
		# line.
		set comment 0
		set firstchar [string index $tline 0]
		foreach c [split $OPTS(-comments) {}] {
		    if { $c eq $firstchar } {
			set comment 1
			break
		    }
		}

		if { !$comment } {
		    if { ([string is true $OPTS(-trim)] \
			      && $tline eq $OPTS(-header)) \
			     || ([string is false $OPTS(-trim)] \
				     && $line eq $OPTS(-header)) } {
			set state "SEPARATOR"
			if { $OPTS(-command) ne {} } {
			    if { [catch {eval [linsert $OPTS(-command) end \
						   [array get HDR]]} err] } {
				${log}::warn "Cannot callback with header\
                                              content: $err"
			    }
			}
		    } else {
			# Arrange for seps to be a list of pairs
			# representing the occurence of the separator and
			# the location of its first occurence in the line.
			set seps {}
			foreach s [split $OPTS(-separators) {}] {
			    set i [string first $s $line]
			    if { $i >= 0 } {
				lappend seps [list $s $i]
			    }
			}

			# Sort to find the first of the possible
			# separators, isolate the string before the
			# separator from the string after the separator,
			# these will form the key and value for the
			# header.
			if { $seps ne {} } {
			    set seps [lsort -integer -index 1 $seps]
			    set idx [lindex [lindex $seps 0] 1]

			    set k [string range $line 0 [expr {$idx-1}]]
			    set v [string range $line [expr {$idx+1}] end]
			    if { [string is true $OPTS(-trim)] } {
				set HDR([string trim $k]) [string trim $v]
			    } else {
				set HDR($k) $v
			    }
			} else {
			    ${log}::notice "Cannot find any header separator\
                                        in '$line'"
			}
		    }
		}
	    }
	    "SEPARATOR" {
		if { ([string is true $OPTS(-trim)] \
			  && $tline ne $OPTS(-header)) \
			 || ([string is false $OPTS(-trim)] \
				 && $line ne $OPTS(-header)) } {
		    set unget $line
		    set state "CONTENT"; # This will arrange for the
		    # next branch of the switch
		    # to trigger
		}
	    }
	    "CONTENT" {
		if { [string is true $OPTS(-trim)] } {
		    lappend content $tline
		} else {
		    lappend content $line
		}
	    }
	}
    }

    # Close the file descriptor when we had opened it.
    if { $fd ne $fd_or_fname } {
	close $fd
    }

    if { $OPTS(-command) eq {} } {
	return [list "headers" [array get HDR] "content" $content]
    } else {
	return $content
    }
}
