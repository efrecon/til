##################
## Module Name     --  make_distro.tcl
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##    Traverses the til directory structure and make the tar distribution.
##
##################
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

# Array Name       --  REVIVE
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Global state of this program.
#
# Contents:
#    arg1	-
#    arg2	-
array set DISTRO {
    finished    0
}

source [file join [file dirname [info script]] .. bin argutil.tcl]
argutil::accesslib tcllib

# Now parse the options and put the result into the global state array
package require cmdline

set options {
    { fexclude.arg "((\\.exe|\\.dll|\\.chm|\\.tpj|~|\\.bak)$)" "Regular expression matching the files to exclude" }
    { dexclude.arg "(CVS)$" "Regular expression matching the directories to exclude" }
    { verbose.arg "warn" "Verbosity Level" }
    { dirs.arg "til" "List of directories" }
    { tar.arg "tar" "Tar executable" }
}

set inited [argutil::initargs DISTRO $options]
if { [catch {cmdline::typedGetoptions argv $options} optlist] != 0 } {
    puts [cmdline::typedUsage $options "accepts the following options:"]
    exit
}
array set DISTRO $optlist

foreach key $inited {
    argutil::makelist DISTRO($key)
}

# Include modules that we depend on.  This is complicated to be able
# to address separately modules in the verbose specification.
argutil::accesslib til
argutil::loadmodules [list diskutil] $DISTRO(verbose)

# Initialise local logging facility
package require logger
set DISTRO(log) [::logger::init revive]
$DISTRO(log)::setlevel $DISTRO(verbose)


argutil::fix_outlog


proc dotraversal { fd dirtop } {
    global DISTRO

    # Get all readable files from the directory using a 8.3 compatible command
    set flist [glob -directory $dirtop -nocomplain -types r -- *]

    $DISTRO(log)::info "Entering $dirtop..."
    foreach fname $flist {
	# Recurse into directories and make sure to
	# create information with the type of the path, the
	# relative name and its modification time.
	if { [file isdirectory $fname] } {
	    if { [catch {regexp $DISTRO(dexclude) $fname} res] == 0 } {
		if { ! $res } {
		    dotraversal $fd $fname
		}
	    } else {
		$DISTRO(log)::warn \
		    "Could not match $fname against $DISTRO(dexclude): $res"
	    }
	} else {
	    if { [catch {regexp $DISTRO(fexclude) $fname} res] == 0 } {
		if { ! $res } {
		    puts $fd $fname
		}
	    } else {
		$DISTRO(log)::warn \
		    "Could not match $fname against $DISTRO(fexclude): $res"
	    }
	}    
    }    
}

foreach dir $DISTRO(dirs) {
    set dir [::diskutil::fname_resolv $dir]
    set tmp [::diskutil::temporary_file "filelist_[file tail $dir]" "lst"]
    set fd [open $tmp "w"]
    fconfigure $fd -translation lf
    dotraversal $fd $dir
    close $fd
    $DISTRO(log)::info "Created file list in $tmp"

    set tarf "[file tail $dir].tgz"
    if { [catch {exec $DISTRO(tar) -cz -f $tarf -T $tmp} res] == 0 } {
	if { [file exists $tarf] } {
	    $DISTRO(log)::notice "Successfully created $tarf"
	} else {
	    $DISTRO(log)::warn "tar successfull, but no tar file created: $res"
	}
    } else {
	$DISTRO(log)::warn "Could not spawn tar: $res"
    }

    file delete $tmp
}


