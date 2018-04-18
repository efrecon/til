##################
## Module Name     --  argutil.tcl
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##     Tiny package free utility module to help the parsing of
##     arguments and to get to packages.
##
## Commands Exported:
##	argutil_accesslib
##	argutil_boolean
##	argutil_makelist
##	argutil_loadmodules
##################
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

package require Tcl 8.2
package provide argutil 1.5

namespace eval ::argutil {
    variable AU
    if { ! [info exists AU] } {
        array set AU {
            levels      {debug info notice warn error critical}
            loglevel    warn
            dft_outfd   stdout
            dateformat  "%d%m%y %H:%M:%S"
            extlog      ""
            outfailed   0
            vbs         "lnk2path.vbs"
            wsh         ""
            links_cache {}
            -maxlinks   10
            -autounwrap off
            -tmpmake    32767
            -tmppfx     "tcltmp_"
            -packed     {.tm .zip .kit}
            -rootdirs   {%scriptdir% %progdir% %libdir%}
            -searchdirs {../../../lib ../../lib ../lib lib ../../.. ../.. .. .}
        }
        variable libdir [file dirname [file normalize [info script]]]
    }
    
    namespace export platform accesslib boolean makelist loadmodules \
            initargs fix_outlog unwrap configure
}


proc ::argutil::configure { args } {
    variable AU
    
    foreach {k v} $args {
        if { [lsearch [array names AU -*] $k] >= 0 } {
            set AU($k) $v
        }
    }
}


# ::argutil::readlnk -- Read the target of a windows shell link
#
#       This command command uses tcom (if possible) or an homebrew
#       solution to attempt reading the content of a windows shell
#       link and returns the target.  The command uses a cache since
#       it is very likely that links are read and requested several
#       times under program initialisation.
#
# Arguments:
#	lnk	Path to shell link
#
# Results:
#	Path to target or error.
#
# Side Effects:
#	Will attempt to load tcom.
proc ::argutil::readlnk { lnk } {
    variable libdir
    variable AU
    
    # Find link in cache, if possible
    foreach {l tgt} $AU(links_cache) {
        if { $l == $lnk } {
            return $tgt
        }
    }
    
    if { ![file exists $lnk] } {
        __log warn "'$lnk' is not an accessible file"
        return -code error "'$lnk' is not an accessible file"
    }
    
    set tgt ""
    if { [catch {package require twapi} err] == 0 } {
        __log debug "'twapi' available, trying most modern method first"
        array set shortcut [::twapi::read_shortcut $lnk]
        if { [info exists shortcut(-path)] } {
            __log info "'$lnk' points to '$shortcut(-path)'"
            if { $shortcut(-path) ne "" } {
                lappend AU(links_cache) $lnk $shortcut(-path)
                return $shortcut(-path)
            }
        }
    } else {
        __log debug "Could not find 'twapi' package: $err"
    }

    if { [catch {package require tcom} err] == 0 } {
        __log debug "'tcom' available trying failsafe method first"
        if { $AU(wsh) eq "" } {
            set AU(wsh) [::tcom::ref createobject "WScript.Shell"]
        }
        set lobj [$AU(wsh) CreateShortcut [file nativename $lnk]]
        set tgt [$lobj TargetPath]
        __log info "'$lnk' points to '$tgt'"
        if { $tgt ne "" } {
            lappend AU(links_cache) $lnk $tgt
            return $tgt
        }
    } else {
        __log debug "Could not find 'tcom' package: $err"
    }
    
    __log debug "Nor 'twapi', nor 'tcom' available or they failed, using link VBS"
    set vbs [file join $libdir $AU(vbs)]
    if { [file exists $vbs] } {
        # Run the VBS script and gather result.
        set cmd "|cscript //nologo \"$vbs\" \"$lnk\""
        set fl [open $cmd]
        set tgt [read $fl]
        if { [catch {close $fl} err] } {
            __log error "Could not read content of link: $lnk"
        } else {
            lappend AU(links_cache) $lnk $tgt
            set tgt [string trim $tgt]
        }
    }
    
    if { $tgt eq "" } {
        set fp [open $lnk]
        fconfigure $fp -encoding binary -translation binary -eofchar {}
        foreach snip [split [read $fp] \x00] {
            set abssnip [file join [file dirname $lnk] $snip]
            if { $snip ne "" && [file exists $abssnip]} {
                __log info "'$abssnip' found in '$lnk', using it as the link!"
                set tgt $snip
                lappend AU(links_cache) $lnk $tgt
                break
            }
        }
        close $fp
    }
    
    return $tgt
}


# ::argutil::logcb -- Register a log output callback
#
#	This procedure registers a command that will be called back
#	every time something is logged via the log facility.  Any
#	number of commands can be registered, they will be called in
#	(their history) sequence.  The commands should take a number
#	of arguments, which will describe what is being logged.  These
#	arguments are: the date, the service, the level and finally
#	the text of the log information itself.  These commands should
#	also return a boolean.  As soon as one command returns 0, the
#	internal log output implemented by the log facility will not
#	take place, which effectively stops output to the console, for
#	example.
#
# Arguments:
#	cmd	Command to be called back at each log (see above for arguments)
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::argutil::logcb { cmd } {
    variable AU
    
    lappend AU(extlog) $cmd
}


proc ::argutil::logrm { cmd } {
    variable AU
    
    set idx [lsearch $AU(extlog) $cmd]
    if { $idx >= 0 } {
        set AU(extlog) [lreplace $AU(extlog) $idx $idx]
    }
}


# ::argutil::__out -- Output log information to console
#
#       This command dumps log information to an opened file
#       descriptor.  The command can also take a file name, in which
#       case it will append the log string to the file.  The
#       implementation of fix_outlog sees to re-route all log dumping
#       from the logger module to this command, that will, as such,
#       act as a central hub.
#
# Arguments:
#	service	Name of the logger service
#	level	Log level at which the output happens
#	str	Information being logged
#	dt	Current date (in seconds since epoch), empty means now
#	fd_nm	File descriptor/name to which to dump, empty means default in AU
#
# Results:
#	None.
#
# Side Effects:
#	Dump to the file descriptor
proc ::argutil::__out { service level str { dt "" } { fd_nm "" } } {
    variable AU
    
    # Store current date in right format
    if { $dt eq "" } {
        set dt [clock seconds]
    }
    set logdt [clock format $dt -format $AU(dateformat)]
    
    # Deliver log callbacks if there are some.  Arrange for "out" to
    # carry whether we should continue with logging to the file or
    # console or whether we should stop once all callbacks have been
    # delivered.
    set out 1
    foreach cmd $AU(extlog) {
        if { [catch {eval $cmd \$logdt $service $level \$str} output] } {
            if { ! $AU(outfailed) } {
                set AU(outfailed) 1
                __out argutil warn \
                        "Could not execute log callback $cmd: $output"
            }
        } else {
            if { ! $output } {
                set out 0
            }
        }
    }
    
    # Don't bother continuing if one of the callbacks has told us not
    # to output anything.
    if { ! $out } {
        return
    }
    
    # Now guess if fd_nm is a file descriptor or the name of a
    # file. If it is a file name, try to open it.
    if { $fd_nm eq "" } {
        set fd_nm $AU(dft_outfd)
    }
    if { [catch {fconfigure $fd_nm}] } {
        # fconfigure will scream if the variable is not a file
        # descriptor, in that case, it is a file name!
        if { [catch {open $fd_nm a+} fd] } {
            if { ! $AU(outfailed) } {
                set AU(outfailed) 1
                __out argutil warn "Cannot open $fd_nm for writing: $fd"
            }
            set fd ""
        }
    } else {
        set fd $fd_nm
    }
    
    # Now that we are here, fd contains where to output the string.
    # It can be empty if we could not open the file when the input was
    # a file name.
    if { $fd ne "" } {
        if { [catch {puts $fd "\[$logdt\] \[$service\] \[$level\] '$str'"}] } {
            if { ! $AU(outfailed) } {
                set AU(outfailed) 1
                __out argutil warn "Cannot write to log file descriptor $fd!"
            }
        }
        if { $fd ne $fd_nm } {
            # Close the output file if the parameter was a name.
            close $fd
        }
    }
}


# ::argutil::__nsExists -- Check if a namespace exists
#
#       Workaround for missing namespace exists in Tcl 8.2 and 8.3.
#       Code taken from the logger package.
#
# Arguments:
#	ns	Name of namespace
#
# Results:
#	1 if the namespace is declared, 0 otherwise
if {[package vcompare [package provide Tcl] 8.4] < 0} {
    proc ::argutil::__nsExists {ns} {
        expr {![catch {namespace parent $ns}]}
    }
} else {
    proc ::argutil::__nsExists {ns} {
        namespace exists $ns
    }
}


# ::argutil::__log -- Internal log function
#
#       This command log internal information from this package
#       through either the logger module, as soon as it is present,
#       either through the output facility.  The command installs a
#       new logger service as soon as the logger module has been
#       loaded.
#
# Arguments:
#	level	Log level for this information
#	str	Information being logged
#
# Results:
#	None.
#
# Side Effects:
#	Will possibly create a new logger service.
proc ::argutil::__log { lvl str } {
    variable AU
    
    if { [__nsExists ::logger] } {
        variable log
        
        if { ! [info exists log] } {
            set log [::logger::init argutil]
            ${log}::setlevel $AU(loglevel)
            fix_outlog argutil
            ${log}::debug "Auto installed logging feature for argutil"
        }
        ${log}::${lvl} $str
    } else {
        set au_lvl [lsearch $AU(levels) $AU(loglevel)]
        set cur_lvl [lsearch $AU(levels) $lvl]
        if { $cur_lvl >= $au_lvl } {
            __out argutil $lvl $str
        }
    }
}


# ::argutil::loglevel -- Set/Get current log level.
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
proc ::argutil::loglevel { { loglvl "" } } {
    variable AU
    variable log
    
    if { $loglvl != "" } {
        if { [info exists log] } {
            if { [catch "${log}::setlevel $loglvl"] == 0 } {
                set AU(loglevel) $loglvl
            }
        } else {
            if { [lsearch $AU(levels) $loglvl] >= 0 } {
                set AU(loglevel) $loglvl
            }
        }
    }
    
    return $AU(loglevel)
}


# ::argutil::resolve_links -- Resolve links
#
#       This command will resolve windows shell links that are
#       contained in a path to their real location on the disk.
#
# Arguments:
#	path	local path to file
#
# Results:
#	Returns a resolved path to the file or an error if the file
#	does not exist
#
# Side Effects:
#	None.
proc ::argutil::resolve_links { path } {
    variable AU
    global tcl_platform
    
    if { $tcl_platform(platform) eq "windows" } {
        # Loops a finite number of time to resolve links to links.
        for { set i 0 } { $i < $AU(-maxlinks) } { incr i } {
            set rp ""
            set sp [file split $path]
            foreach d $sp {
                set jp [file join $rp $d]
                if { [string toupper [file extension $jp]] eq ".LNK" } {
                    set rp [file join $rp [readlnk $jp]]
                } elseif { [file exists ${jp}.lnk] } {
                    set rp [file join $rp [readlnk ${jp}.lnk]]
                } else {
                    set rp $jp
                }
            }
            if { $rp eq $path } {
                break
            }
            set path $rp
        }
        return $rp
    } else {
        return $path
    }
}


# ::argutil::resolve_path -- Resolve links in list of path
#
#       This command modifies the variable passed as an argument,
#       considering it a path (like auto_path) and resolving all its items
#       using the resolve_links command.
#
# Arguments:
#	path_p	Pointer to list of paths
#
# Results:
#	None.
#
# Side Effects:
#	Will modify the content of the variable passed as an argument.
proc ::argutil::resolve_path { path_p } {
    upvar $path_p path
    
    set res ""
    foreach p $path {
        lappend res [resolve_links $p]
    }
    set path $res
}


# ::argutil::platform -- Return platform unique identification
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
proc ::argutil::platform {} {
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


# ::argutil::__copytmp -- Copy a (binary) library to a temporary location
#
#	This function copies a library containing binaries element out
#	of the current starkit and into a temporary location so that
#	the starkit will be able to access the binary elements at
#	run-time in a transparent manner.  The command avoids copying
#	the library if it had been copied at a previous run through
#	creating temporary directories which name depend uniquely on
#	the binary starkit.  Old remnants of previous installations of
#	the same starkit are automatically cleaned away from the
#	temporary directory.
#
# Arguments:
#	libdir	Path to top library directory (in starkit vfs!)
#
# Results:
#	Returns the path to the top of the temporary location
#
# Side Effects:
#	None.
proc ::argutil::__copytmp { libdir } {
    variable AU
    
    # XXX: The current implementation will copy all code to the
    # temporary directory which might expose tcl code from a private
    # starkit onto the local disk.  One solution to remedy to this
    # problem would be to obfuscate gently/encrypt the code to make it
    # less readable and less obvious that this is program code.
    
    set ourbin [file tail $::starkit::topdir]
    
    # Make a unique number that identifies this binary (via its raw
    # name (without considering the directory) and its "compilation"
    # time).
    set unique 0
    for { set i 0 } { $i < [string length $ourbin] } { incr i } {
        set c [scan [string index $ourbin $i] %c]
        set unique [expr ($unique + $c) % $AU(-tmpmake)]
    }
    set m [file mtime [file join $::starkit::topdir main.tcl]]
    set unique [expr ($unique + $m) % $AU(-tmpmake)]
    
    # Now recursively copy the content of the library directory passed
    # as an argument into a temporary directory that is made unique
    # using the integer computed above.
    set systmp [::fileutil::tempdir]
    set tmpdir [file join $systmp "$AU(-tmppfx)${ourbin}_${unique}"]
    file mkdir $tmpdir
    set tmpdir [file join $tmpdir [file tail $libdir]]
    if { ! [file exists $tmpdir] } {
    __log notice "Copying source from $libdir to $tmpdir\
                so as to be able to access binaries"
        file copy -force $libdir [file dirname $tmpdir]
    }
    
    # Finally, remove old possible installations for that binary.
    # This allows us not to pollute the temporary folder too much.
    foreach d [glob -nocomplain -directory $systmp -tails \
            "$AU(-tmppfx)${ourbin}_*"] {
        if { $d ne "$AU(-tmppfx)${ourbin}_${unique}" } {
            __log notice "Removing old temporary binaries at $d"
            catch {file delete -force [file join $systmp $d]}
        }
    }
    
    return $tmpdir
}


# ::argutil::__isexec -- Test if a file is an executable
#
#	This procedure uses heuristics such as the extension and/or
#	the file attributes to detect if a file is an
#	executable/dynamic library or not.  This command is used to
#	detect when we should copy library implementation trees out of
#	a starpack.
#
# Arguments:
#	fname	Path to file
#
# Results:
#	Return 1 if the file is an executable, 0 otherwise.
#
# Side Effects:
#	None.
proc ::argutil::__isexec { fname } {
    global tcl_platform
    
    set ext [string tolower [file extension $fname]]
    if { $tcl_platform(platform) == "unix" } {
        return [expr {$ext == ".so" || [file executable $fname]}]
    } else {
        return [expr {$ext == ".exe" || $ext == ".dll"}]
    }
    return 0; # Never reached
}


proc ::argutil::__forbidden { p } {
    variable AU
    global tcl_platform
    
    if { $tcl_platform(platform) ne "windows" } {
        # On UNIX-like systems, we don't want to start digging
        # into the OS installed libraries.
        set d [file dirname [file normalize $p]]
        foreach ptn [list "/" "/usr" "/usr/lib*" "/var/lib*"] {
            if { [string match $ptn $d] } {
                return 1
            }
        }
    }
    return 0
}


# ::argutil::searchlib -- Search lib at version number
#
#	This procedure searches for the implementation of a library
#	within a number of possible directories.  The procedure can
#	either look for the library at a given version or for the one
#	that is at the highest (latest) version.  The algorithm
#	supposes that the version number is included in the name of
#	the directory containing the library after the name of the
#	library itself (e.g. tcllib1.9).
#
# Arguments:
#	libname	Name of library to look for.
#	paths	List of directory paths to look within
#	version	Specific version of the library to look for, empty means latest
#
# Results:
#	Return a list where the first argument is the version number
#	and the second argument the path the library at that version
#	number.
#
# Side Effects:
#	None.
proc ::argutil::searchlib { libname paths { version "" } } {
    variable AU
    global tcl_platform
    
    __log debug "Looking for '$libname' in '$paths'"
    
    # Now, try to find the ones that contain a sub named $libname with a
    # version number or not.
    set vernum ""
    set libdir ""
    if { $version == "" } {
        # If version number was unspecified, look for the maximum one
        # from the search path.  First, we look for directories which
        # name match $libname* and list these in libpaths.
        set libpaths ""
        foreach p $paths {
            if { [__forbidden $p] } {
                continue
            }
            set libpath [file join [resolve_links $p] "$libname"]
            append libpath "*"
            __log debug "Checking directory $libpath..."
            if { [catch {glob $libpath} paths] == 0 } {
                foreach dir $paths {
                    if { $tcl_platform(platform) ne "windows" \
                                && [string match "*.lnk" [string tolower $dir]] } {
                        continue
                    }
                    # Resolve and normalize directory
                    set rdir [resolve_links $dir]
                    if { [catch {file normalize $rdir} ndir] == 0 } {
                        set rdir $ndir
                    }
                    if { [__forbidden $rdir] } {
                        continue
                    }
                    if { [file exists $rdir]\
                                && [lsearch $libpaths $rdir] < 0 } {
                        __log debug "Found possible directory: $rdir"
                        lappend libpaths $rdir
                    }
                }
            }
        }
        
        # Then we extract the version number from the directory name
        # and build an indexing list maxver_l into the directory
        # list. We attempt to extract the version number simply by
        # silently advancing to the first digit in the path and
        # considering that all the remaining characters form the
        # version number.
        set maxver_l ""
        set idx 0
        foreach p $libpaths {
            # Find the name of the library in the path
            set l_idx [string last $libname $p]
            if { $l_idx >= 0 } {
                # Advance to first non alpha character, the substring
                # is the name of the library (as analyzed from the
                # directory name)
                set libnamefromdir [string range $p $l_idx end]
                
                set unpackedname $libnamefromdir
                foreach ext $AU(-packed) {
                    if { [file extension $libnamefromdir] eq $ext } {
                        set unpackedname [file rootname $libnamefromdir]
                    }
                }
                set rawlibname $unpackedname
                
                set vercontainer [string map [list $libname ""] $unpackedname]
                # Try to advance to first digit and consider all remaining
                # characters (including the digit) as the version number
                set v_num ""
                for {set i 0} { $i < [string length $vercontainer] } {incr i} {
                    if { [string is digit [string index $vercontainer $i]] } {
                        set v_num \
                                [string trim [string range $vercontainer $i end]]
                        if { [regexp {^[_.\-\d]+$} $v_num] } {
                            break
                        } else {
                            # BACKTRACK, this wasn't a version but
                            # rather a number in the middle of the
                            # filename...
                            set v_num ""
                        }
                    }
                }
                __log debug "Analysed $libnamefromdir at v. $v_num from $p"
                
                # If we found something that had a valid version
                # number or if we found a library without any version
                # number (strict match on the name) add it to the list
                # of paths to consider.
                __log debug "Found one match for $libname: $p, v. $v_num"
                lappend maxver_l [list $v_num $idx]
            }
            incr idx
        }
        
        # We sort the indexing list and the top index is the directory
        # at the maximum version number.
        if { [llength $maxver_l] > 0 } {
            set maxver_l [lsort -decreasing -index 0 $maxver_l]
            set vernum [lindex [lindex $maxver_l 0] 0]
            set libdir [lindex $libpaths [lindex [lindex $maxver_l 0] 1]]
        }
    } else {
        # If the version number was forced from the outside, look for
        # these directories only.
        foreach p $paths {
            set libpath [resolve_links [file join $p "${libname}${version}"]]
            if { [file isdirectory $libpath] } {
                set libdir $libpath
                set vernum $version
                break
            }
        }
    }
    
    __log info "Found $libname in $libdir at version $vernum"
    
    return [list "$vernum" $libdir]
}


# ::argutil::accesslib -- Intelligently modify load path for accessing lib
#
#	Arrange so that the library which name (and version number)
#	are passed as arguments can be accessed through package
#	require calls. If the version number is empty, the library
#	which maximum version number (as embedded in the directory
#	name) will be chosen.  The version number of the library is
#	return (empty string on error) and the global package loading
#	path is amended.  This command will also auto-extract library
#	directories that contain binary extensions into a temporary
#	space when running from a starkit.
#
# Arguments:
#	libname	Name of the library (for example tcllib)
#	version	Version number of the library (for example 1.6.1)
#	first	Prepend path to library to auto_path (instead of append)
#
# Results:
#	Returns a list which first argument is the (guessed) version
#	number of the library (from the directory name) and the second
#	argument is a list of the directories that were added to the
#	auto_path.
#
# Side Effects:
#	Modifies auto_path.
proc ::argutil::accesslib { libname { version "" } { first 0 } } {
    global auto_path tcl_platform argv0
    variable libdir
    variable AU
    
    # Build a list of library path from within which we should look
    # for a $libname sub directory.
    set path_search {}
    foreach rd $AU(-rootdirs) {
        set rd [string map [list %scriptdir% $libdir \
                %progdir% [file dirname $argv0] \
                %libdir% [info library]] $rd]
        foreach d $AU(-searchdirs) {
            lappend path_search [file join $rd $d]
        }
    }
    
    # Find library in search path
    foreach {vernum libdir} [searchlib $libname $path_search $version] break
    
    # Automatically unwrap library directories that contain dynamic
    # libraries or executables, and see to load the library from there
    # instead.
    if { $libdir != "" && [string is true $AU(-autounwrap)] } {
        if { [info exists ::starkit::topdir] } {
            if { [catch {package require fileutil}] == 0 } {
                set binaries [::fileutil::find $libdir ::argutil::__isexec]
                if {0 } {
                    set binaries [::fileutil::findByPattern $libdir -regexp -- \
                            "(dll|so|exe)$"]
                }
                if { [llength $binaries] > 0 } {
                    # Force the directory to be first in the auto
                    # loading path, so as to be sure to get hold of
                    # the local copy.
                    set first 1
                    set libdir [__copytmp $libdir]
                }
            } else {
        __log warn "Cannot find fileutil, will not unwrap $libdir\
                        if necessary"
            }
        }
    }
    
    # If we have found one directory with the $libname, add it to our
    # search path and return the version number.
    set added_dirs ""
    if { $libdir != "" } {
        if { [catch "file normalize $libdir" fullpath] != 0 } {
            set fullpath $libdir
        }
        
        set platform_dirs [list \
                [file join $fullpath $tcl_platform(platform)] \
                [file join $fullpath [platform]]]
        foreach d $platform_dirs {
            if { [file exists $d] && [file isdirectory $d] } {
                if { $first } {
                    set auto_path [linsert $auto_path 1 $d]
                } else {
                    lappend auto_path $d
                }
                if { [catch {::tcl::tm::add $d} err] } {
                    __log warn "Could not add $d to module path: $err"
                }
                lappend added_dirs $d
            }
        }
        
        if { [llength $added_dirs] == 0 } {
            if { $first } {
                set auto_path [linsert $auto_path 0 $fullpath]
            } else {
                lappend auto_path $fullpath
            }
            if { [catch {::tcl::tm::add $fullpath} err] } {
                __log warn "Could not add $fullpath to module path: $err"
            }
            lappend added_dirs $fullpath
        }
        
        __log info "Added $added_dirs to auto loading path"
    }
    
    return [list $vernum $added_dirs]
}


# ::argutil::boolean -- Modifies array variable for testing as boolean
#
#	Make sure the array variable (typically set by cmdline
#	package) can be used as a boolean.  Out of the command line
#	parsing routine, the index will only exist if the argument was
#	specified on the command line.  This command make sure it
#	always exists and initialises it to 0 or 1 as it
#	should.
#
# Arguments:
#	a_name_p	Name of array to tweak
#	a_index	Variable index to arrange
#
# Results:
#	None.
#
# Side Effects:
#	Modifies the array pointed at by a_name_p so that it contains
#	the boolean and is initialised correctly.
proc ::argutil::boolean { a_name_p a_index } {
    upvar $a_name_p a_name
    
    if { [array names a_name $a_index] == $a_index } {
        set a_name($a_index) 1
    } else {
        set a_name($a_index) 0
    }
}



# ::argutil::makelist -- Make list out of badly formed arguments.
#
#	This command detects wether a variable starts with a curly
#	brace and removes in that case the leading and ending braces
#	to make it a first-level list.
#
# Arguments:
#	var_p	Variable to test and modify to a first-level list.
#
# Results:
#	None.
#
# Side Effects:
#	Modifies the variable.
proc ::argutil::makelist { var_p } {
    upvar $var_p var
    
    if { [string index $var 0] == "\{" } {
        set var [string range $var 1 end-1]
    }
}



# ::argutil::loadmodules -- Load TIL modules
#
#	Load a number of modules from the til and initialise their
#	loglevel. verbose can either be a list of one argument, in
#	which case all modules will be initialised at that level, or
#	an array set-type list where the first argument is the name of
#	the module and the second argument is the verbosity level for
#	that module.
#
# Arguments:
#	modules	List of modules to load
#	verbose	Verbosity level for modules (global or specific, see above)
#
# Results:
#	A list containing the version number of each package that was
#	loaded
#
# Side Effects:
#	Will actively load the modules.
proc ::argutil::loadmodules { modules { verbose "" } } {
    # Make sure the verbosity specification is a list.
    makelist verbose
    
    # Require all modules to load them into memory.
    set vernums ""
    foreach module $modules {
        if { [string first ":" $module] >= 0 } {
            foreach {module version} [split $module ":"] break
            lappend vernums [package require $module $version]
        } elseif { [string first "!" $module] >= 0 } {
            foreach {module version} [split $module "!"] break
            lappend vernums [package require -exact $module $version]
        } else {
            lappend vernums [package require $module]
        }
        __log info "Loaded package $module, v. [lindex $vernums end]"
    }
    
    if { $verbose ne "" } {
        if { [llength $verbose] == 1 } {
            # If verbose contains one argument only, tell all modules to
            # be at that level, do this only if they support the loglevel
            # command.
            foreach module $modules {
                if { [info commands ::${module}::loglevel] != "" } {
                    ::${module}::loglevel $verbose
                    __log debug "Setting log level of $module to $verbose"
                } else {
                    __log warn "Cannot change loglevel for $module to $verbose"
                }
            }
        } else {
            # Otherwise do this specifically for each module which name
            # was present in the verbose list.
            for { set i 0 } { $i < [llength $verbose] } { incr i 2 } {
                set module [lindex $verbose $i]
                set level [lindex $verbose [expr $i + 1]]
                if { [info commands ::${module}::loglevel] != "" } {
                    ::${module}::loglevel $level
                    __log debug "Setting log level of $module to $verbose"
                } else {
                    __log warn "Cannot change loglevel for $module to $verbose"
                }
            }
        }
    }
    
    return $vernums
}



# ::argutil::initargs -- Initialises empty command-line opts to empty strings
#
#	This command takes an array and a command-line option
#	description array and initialises all string arguments to an
#	empty string. Since it actively modifies the array, this
#	command should be called before parsing the options!  It
#	returns the list of keys that were initialised in the array.
#
# Arguments:
#	array_p	Name of global array to initialise
#	optdesc	Command-line option description
#
# Results:
#	A list containing indices that were initialised
#
# Side Effects:
#	Will actively modify the array
proc ::argutil::initargs { array_p optdescr } {
    upvar $array_p array
    
    set inits ""
    foreach optspec $optdescr {
        set prm [lindex $optspec 0]
        set dot [string first "." $prm]
        if { $dot >= 0 } {
            set key [string range $prm 0 [expr {$dot - 1}]]
            if { [regexp "^.*\\.arg$" $prm] \
                        || [regexp "^.*\\.\\(.*\\)$" $prm] } {
                __log debug "Initialised ${array_p}($key) to empty string"
                set array($key) ""
                lappend inits $key
            }
        }
    }
    
    return $inits
}


# ::argutil::options -- Return known options
#
#	This command takes a command-line option description array
#	(designed for the cmdline parser) and computes the list of
#	known options to the program.
#
# Arguments:
#	optdesc	Command-line option description
#
# Results:
#	A list with all the options that are allowed for the program
#
# Side Effects:
#	None
proc ::argutil::options { optdescr } {
    set options [list]
    foreach optspec $optdescr {
        set prm [lindex $optspec 0]
        set dot [string first "." $prm]
        if { $dot >= 0 } {
            lappend options [string range $prm 0 [expr {$dot - 1}]]
        } else {
            lappend options $prm
        }
    }
    
    return $options
}


# ::argutil::fix_outlog -- Fix log output for better date
#
#	Fix so that the date format of the log output for the
#	different logger services passed as a parameter is not as
#	horribly long.
#
# Arguments:
#	services	List of services to fix, empty means all of them!
#
# Results:
#	A list containing the services that were modified.
#
# Side Effects:
#	Will actively modify the output procedure in the logger module.
proc ::argutil::fix_outlog { { services {} } } {
    variable AU
    
    if { [llength $services] == 0 } {
        set services [::logger::services]
    }
    
    # For all the services, declare a number of output procedures (one
    # for each log level supported by the module, and see to install
    # this procedure as the log procedure for that level.
    if { [info commands ::logger::servicecmd] ne "" } {
        foreach svc $services {
            foreach lvl [::logger::levels] {
                set procname "[::logger::servicecmd $svc]::__AUout_$lvl"
                set procdecl "proc $procname { txt } \{ ::argutil::__out $svc $lvl \$txt \}"
                eval $procdecl
                set root [::logger::servicecmd $svc]
                ${root}::logproc $lvl ${root}::__AUout_$lvl
            }
        }
        __log info "Fixed log output for services: $services"
    }
    
    return $services
}

proc ::argutil::unwrap { wrapbin } {
    variable AU
    
    set rawname [file tail $wrapbin]
    set dstbin [file join [::diskutil::platform_tmp] $rawname]
    
    if { [file exists $dstbin] } {
        if { [file size $dstbin] == [file size $wrapbin] } {
            set shouldcopy 0
        } else {
            set shouldcopy 1
        }
    } else {
        set shouldcopy 1
    }
    if { $shouldcopy } {
        __log info "Copying wrapped binary $wrapbin to $dstbin"
        if { [catch {file copy -force -- $wrapbin $dstbin} err] } {
            __log error "Could not copy $wrapbin into $dstbin: $err"
            return ""
        }
    }
    return $dstbin
}

