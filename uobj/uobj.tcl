# uobj.tcl -- Micro-Object system
#
#	This module contains procedures to build pseudo-objects on top
#	of the standard namespace system, without any other object
#	orientation than creation through dash prefix options and
#	regular arrays.
#
# Copyright (c) 2004-2006 by the Swedish Institute of Computer Science.
#
# See the file 'license.terms' for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

package require Tcl 8.4
package require logger
package require diskutil

namespace eval ::uobj {
    variable UOBJ
    if { ! [info exists UOBJ] } {
        array set UOBJ {
            comments         "\#!;"
            loglevel         warn
            header           "\n"
            indent           "  "
            resolv_access    0
            resolv_gc_period 10
        }
        variable log [::logger::init [string trimleft [namespace current] ::]]
        variable libdir [file dirname [file normalize [info script]]]
        ${log}::setlevel $UOBJ(loglevel)
    }
    namespace export loglevel
}


# ::uobj::lassign -- Assign variables from values in list
#
#	This is an implementation of lassign for older version of tcl
#
# Arguments:
#	values	list of values
#	args	variables to set to each item of the value list.
#
# Results:
#	If there are more list elements than variables, a list of unassigned
#	elements is returned.
#
# Side Effects:
#	None.
if {[catch {lassign {}}]} {
   proc ::uobj::lassign {values args} {
       uplevel 1 [list foreach $args [linsert $values end {}] break]
       lrange $values [llength $args] end
   }
}


# ::uobj::loglevel -- Set/Get current log level.
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
proc ::uobj::loglevel { { loglvl "" } } {
    variable UOBJ
    variable log
    
    if { $loglvl != "" } {
        if { [catch "${log}::setlevel $loglvl"] == 0 } {
            set UOBJ(loglevel) $loglvl
        }
    }
    
    return $UOBJ(loglevel)
}


# ::uobj::logsupport -- Creates appropriate loglevel procedure
#
#	This procedure installs the necessary procedure and variables
#	to enhance a module with logger support.  The procedure
#	installed will be called loglevel and will either set or
#	return the loglevel whether it is called with an argument or
#	not.  The module is supposed to have a global array for state
#	storage and will also be donated with a global array for
#	storing the logger context.
#
# Arguments:
#	nmspace	Namespace of module to enhance with logger support
#	lvlstr	Name of global array in external module that contains context
#	dftlvl	Default log level for module, if not already set
#	logstr	Name of global variable in external module for logger context
#	lvlidx	Index in global array for current level storage
#
# Results:
#	None
#
# Side Effects:
#	Will create procedures and variables in external namespace!
proc ::uobj::install_log { nmspace lvlstr {dftlvl "warn"} {logstr "log"} {lvlidx "loglevel"}} {
    variable UOBJ
    variable log
    
    # Trim the namespace name in case
    set nmspace [string trimleft $nmspace ::]
    ${log}::info "Adding logger support to $nmspace (defaulting to '$dftlvl')"
    
    # Create the logger context and store it in the variable "logstr"
    # in the external namespace module
    if { ! [info exists ::${nmspace}::${logstr}] } {
        set ::${nmspace}::${logstr} [::logger::init $nmspace]
        ${log}::debug "Started logger as [set ::${nmspace}::${logstr}]"
    }
    
    # Create the index in the global array of the external namespace
    # module and initialise its associated logger module.
    if { ! [info exists ::${nmspace}::${lvlstr}($lvlidx)] } {
        set ::${nmspace}::${lvlstr}($lvlidx) $dftlvl
        set l [set ::${nmspace}::${logstr}]
        ${l}::setlevel $dftlvl
        ${log}::debug \
                "Created current level storage as ::${nmspace}::${lvlstr}($lvlidx)"
    }
    
    # Now create loglevel procedure in external module.
    interp alias {} ::${nmspace}::loglevel {} \
            [namespace current]::__loglevel  $nmspace $lvlstr $logstr $lvlidx
    return
    
    eval [string map [list @nmspace@ $nmspace \
            @lvlstore@ $lvlstr \
            @logstore@ $logstr \
            @lvlidx@ $lvlidx] {
        proc ::@nmspace@::loglevel { {loglvl ""} } {
            variable @lvlstore@
            variable @logstore@
            
            if { $loglvl ne "" } {
                if { [catch {${@logstore@}::setlevel $loglvl}] == 0 } {
                    set @lvlstore@(@lvlidx@) $loglvl
                }
            }
            
            return $@lvlstore@(@lvlidx@)
        }
    }]
}

proc ::uobj::__loglevel { nmspace lvlstore logstore lvlidx {loglvl ""}} {
    if { $loglvl ne "" } {
        if { [catch {[set ${nmspace}::${logstore}]::setlevel $loglvl}] == 0 } {
            set ${nmspace}::${lvlstore}($lvlidx) $loglvl
        }
    }
    return [set ${nmspace}::${lvlstore}($lvlidx)]
}


# ::uobj::install_defaults -- Create module defaults get/set
#
#	This procedure installs a procedure called "defaults" in the
#	namespace passed as an argument.  The procedure bridges the
#	configuration facilities below and offer to get and/or set
#	options for the whole module
#
# Arguments:
#	nmspace	Namespace of module to enhance with defaults support
#	dftstr	Name of global array in external module that contains context
#
# Results:
#	(see config below)
#
# Side Effects:
#	None.
proc ::uobj::install_defaults { nmspace dftstr } {
    variable UOBJ
    variable log
    
    # Trim the namespace name in case
    set nmspace [string trimleft $nmspace ::]
    ${log}::info "Adding support for defaults options to $nmspace"
    
    interp alias {} ::${nmspace}::defaults {} \
            [namespace current]::config ::${nmspace}::${dftstr} [list "-*"]
    
    return
    
    eval [string map [list @nmspace@ $nmspace @dftstr@ $dftstr] {
        proc ::@nmspace@::defaults { args } {
            variable @dftstr@
            
            return [eval ::uobj::config @dftstr@ "-*" $args]
        }
    }]
}


# ::uobj::config -- Options get/set configurator
#
#	This procedure is a helper to get or set options in an
#	"object", i.e. a regular Tcl array.  The options concerned are
#	all those that will match the list of patterns passed (which
#	often will be "-*", i.e. all indices in the array starting
#	with a dash).  Called with no arguments, the configurator will
#	return a list containing all the options and their value
#	(ready for an array set command).  Called with one single
#	argument and if this argument is an option, the helper will
#	return the value of that option only if it is one.  Otherwise,
#	the arguments is considered to be a list of pairs (array set
#	style) of options and values, which will be set into the
#	incoming object.
#
# Arguments:
#	obj_p	"Pointer" to object (i.e. regular Tcl array)
#	pattns	List of patterns describing the options in the array
#	args	None, one or list of pairs, as explained above
#
# Results:
#	List of options with their value, current value for an option
#	or nothing depending on the action that was taken.
#
# Side Effects:
#	Will sometimes modify the array which name is passed as a
#	parameter.
proc ::uobj::config { obj_p pattns args } {
    variable UOBJ
    variable log
    
    # Get to the array.
    upvar $obj_p Object
    
    # See to have all matching options concerned by the operation in
    # "alloptions"
    set alloptions [list]
    foreach ptn $pattns {
        set alloptions [concat $alloptions [array names Object $ptn]]
    }
    set alloptions [lsort -unique $alloptions]
    
    # Now get or set options, depending on args
    set result ""
    if { [llength $args] == 0 } {          ;# Return all results
        foreach name $alloptions {
            lappend result $name $Object($name)
        }
    } else {
        foreach {opt value} $args {        ;# Get one or set some
            if { [lsearch $alloptions $opt] == -1 } {
                return -code error \
                        "Unknown option $opt, must be: [join $alloptions ", " ]"
            }
            if { [llength $args] == 1 } {  ;# Get one config value
                set result $Object($opt)
                break
            }
            set Object($opt) $value        ;# Set the config value
        }
    }
    
    return $result
}


# ::uobj::configure -- Pure argument configuration only
#
#	This procedure considers that the object passed as argument
#	has a set of options led by dashes.  These options and only
#	these options can be modified by the list of arguments passed.
#
# Arguments:
#	obj_p	Pointer to object
#	args	List of options and values to push into the object
#
# Results:
#	Return the list of options that were effectively changed, or
#	an error for unknown options.
#
# Side Effects:
#	Will actively modify the array pointed by obj_p
proc ::uobj::configure { obj_p args } {
    variable UOBJ
    variable log
    
    # Get to the array
    upvar $obj_p Object
    
    # Set the options from the arguments.
    set res [list]
    set opts [array names Object -*]
    foreach {opt val} $args {
        if { [lsearch $opts $opt] >= 0 } {
            set oldval $Object($opt)
            if { $oldval ne $val } {
                set Object($opt) $val
                lappend res $opt
            }
        } else {
            return -code error \
                    "Unknown options $opt, should be: [join $opts ", "]"
        }
    }
    
    return $res
}


# ::uobj::cget -- Option get
#
#	Get an option out of an object
#
# Arguments:
#	obj_p	Pointer to object
#	opt	Option to get from object
#
# Results:
#	Return the value of the option or an error if it is an unknown
#	option
#
# Side Effects:
#	None.
proc ::uobj::cget { obj_p opt } {
    variable UOBJ
    variable log
    
    # Get to the array
    upvar $obj_p Object
    
    # Return the value of the option or an error.
    set opts [array names Object -*]
    if { [lsearch $opts $opt] >= 0 } {
        return $Object($opt)
    } else {
        return -code error \
                "Unknown options $opt, should be: [join $opts ", "]"
    }
}


# ::uobj::inherit -- Inherit options/values between arrays
#
#	This procedure copies all options (and their respective
#	values) from the parent array into the child array.  The
#	options that are copied are the ones that matches all the list
#	of patterns that is passed as a parameter.
#
# Arguments:
#	parent_p	"pointer" to parent array
#	child_p 	"pointer" to child array
#	patterns	List of patterns for options to be copied
#	restrict	List of patterns for options not to be copied
#
# Results:
#	None
#
# Side Effects:
#	None.
proc ::uobj::inherit { parent_p child_p {patterns "-*"} {restrict ""}} {
    variable UOBJ
    variable log
    
    upvar $parent_p parent
    upvar $child_p child
    
    foreach ptn $patterns {
        foreach opt [array names parent $ptn] {
            set copy 1
            foreach r $restrict {
                if { [string match $r $opt] } {
                    set copy 0
                }
            }
            if { $copy } {
                set child($opt) $parent($opt)
            }
        }
    }
}


# ::uobj::diff -- Difference between two objects.
#
#       Computes the list of options that are different between two
#       objects.  Both objects are taken as being of the same type,
#       but no specific check is performed.
#
# Arguments:
#       obj1_p		"pointer" to first object.
#       obj2_p		"pointer" to second object.
#	patterns	List of patterns for options to be tested
#	restrict	List of patterns for options not to be tested
#
# Results:
#       Return the list of options for which the value in object one
#       was different from object two.  Difference is done as a strict
#       string difference.
#
# Side Effects:
#       None.
proc ::uobj::diff { obj1_p obj2_p {patterns "-*"} {restrict ""}} {
    variable UOBJ
    variable log
    
    upvar $obj1_p obj1
    upvar $obj2_p obj2
    
    set diff [list]
    foreach ptn $patterns {
        foreach opt [array names obj1 $ptn] {
            set testopt 1
            foreach r $restrict {
                if { [string match $r $opt] } {
                    set testopt 0
                }
            }
            if { $testopt } {
                if { $obj1($opt) ne $obj2($opt) } {
                    lappend diff $opt
                }
            }
        }
    }
    
    return $diff
}


proc ::uobj::stacktrace {} {
    variable UOBJ
    variable log
    
    set stack $UOBJ(header)
    for {set i 1} {$i < [info level]} {incr i} {
        append stack [string repeat $UOBJ(indent) $i][info level -$i]
        append stack "\n"
    }
    return $stack
}


proc ::uobj::mdefault { key val } {
    variable UOBJ
    variable log
    
    set k [split $key "."]
    
    if { [llength $k] >= 2 } {
        set opt "-[lindex $k end]"
        set module [join [lrange $k 0 end-1] "::"]
        if { [string range $module 0 1] ne "::" } {
            set nmspace "::$module"
        } else {
            set nmspace "$module"
        }
        if { [namespace exists $nmspace] } {
            if { [info commands ${nmspace}::defaults] ne "" } {
                if { [catch {${nmspace}::defaults $opt $val} err] } {
                    ${log}::warn "Could not set ${key} to $val: $err"
                } else {
                    return [list ${module} ${opt}]
                }
            } else {
                ${log}::warn "No defaults for $module"
            }
        } else {
            ${log}::warn "ERROR, no namespace $module"
        }
    } else {
	    ${log}::warn "Configuration specification $key has\
                erroneous format"
    }
    
    return {}
}

# ::uobj::readconfig -- Read and apply configuration parameters.
#
#	This procedure reads configuration parameters from a file
#	containing, apart from comments, lines of key value settings.
#	The keys are understood to be divided by a "." separator where
#	the left part is the name of (any module) and the right part
#	the name of a dash option, without the leading dash.  If the
#	module implements a "defaults" command and support the option,
#	the value from the file will automatically be given to the
#	default setting.  This allows to set defaults settings for a
#	number of modules in a go.
#
# Arguments:
#	fname	Full path to file
#
# Results:
#	Returns a list of pairs module option for all options that
#	were effectively set to a value.
#
# Side Effects:
#	None.
proc ::uobj::readconfig { fname } {
    variable UOBJ
    variable log
    
    set dfts [list]
    ${log}::info "Reading configuration file from '$fname'"
    if { [catch {open $fname} fd] } {
        ${log}::warn "Could not read configuration from '$fname': $fd"
    } else {
        while { ! [eof $fd] } {
            set line [string trim [gets $fd]]
            if { $line ne "" } {
                set firstchar [string index $line 0]
                if { [string first $firstchar $UOBJ(comments)] < 0 } {
                    set key [lindex $line 0]
                    set val [lindex $line 1]
                    lassign [mdefault $key $val] module opt
                    if { $module ne "" } {
                        lappend dfts $module $opt
                    }
                }
            }
        }
        close $fd
    }
    
    return $dfts
}


# ::uobj::serialize -- Serialize array to file
#
#	This procedure serializes the content of an "object" to a
#	file.  It provides a mechanism to select which part of the
#	array should be written to the file.  All array elements that
#	matches one of the patterns will be written down to the file
#	if they do not match any of the restricting patterns.
#
# Arguments:
#	ary_p	"pointer" to the array
#	fd_or_n Either an opened file descriptor or the path to a file
#	        that will be overwritten with content.
#	pterns	List of patterns for elements to be copied
#	rstrict	List of patterns for options not to be copied
#
# Results:
#	Return the list of indices that were written,  an error on errors
#
# Side Effects:
#	Will overwrite the file if a file name is specified as second
#	argument
proc ::uobj::serialize { ary_p fd_or_n {pterns "-*"} {rstrict ""} } {
    variable UOBJ
    variable log
    
    # Guess if second argument is an opened file descriptor or a file
    # name.  If it is a file name, open it.  Always make sure fd is a
    # file descriptor to which we will write.
    if { [catch {fconfigure $fd_or_n}] } {
        ${log}::info "Opening $fd_or_n for serialization"
        if { [catch {open $fd_or_n w} fd] } {
            ${log}::warn "Could not open $fd_or_n for writing: $fd"
            return -code error "Could not open $fd_or_n for writing: $fd"
        }
    } else {
        set fd $fd_or_n
    }
    
    # Dump to <fd> all the indices matching the allowance and
    # restriction patterns.  Dump their value aside.  Account for
    # which indices are dumped.
    set dumped [list]
    upvar $ary_p OBJECT
    foreach ptn $pterns {
        foreach idx [array names OBJECT $ptn] {
            set dump 1
            foreach r $rstrict {
                if { [string match $r $idx] } {
                    set dump 0
                }
            }
            if { $dump } {
                puts $fd "$idx \{$OBJECT($idx)\}"
                lappend dumped $idx
            }
        }
    }
    
    # Close the file if the second parameter was a file name.
    if { $fd ne $fd_or_n } {
        close $fd
    }
    
    return $dumped
}


# ::uobj::deserialize -- DeSerialize array from file
#
#	This procedure desserializes the content of an "object" from a
#	file.  It provides a mechanism to select which part of the
#	array should be read from the file.  All array elements that
#	matches one of the patterns will be written down to the array
#	if they do not match any of the restricting patterns.
#
# Arguments:
#	ary_p	"pointer" to the array
#	fd_or_n Either an opened file descriptor or the path to a file
#	        that will be overwritten with content.
#	pterns	List of patterns for elements to be copied
#	rstrict	List of patterns for options not to be copied
#
# Results:
#	Return the list of indices that were written to the array, an
#	error on errors
#
# Side Effects:
#	None
proc ::uobj::deserialize { ary_p fd_or_n {pterns "-*"} {rstrict ""} } {
    variable UOBJ
    variable log
    
    # Guess if second argument is an opened file descriptor or a file
    # name.  If it is a file name, open it.  Always make sure fd is a
    # file descriptor to which we will write.
    if { [catch {fconfigure $fd_or_n}] } {
        ${log}::info "Opening $fd_or_n for deserialization"
        if { [catch {open $fd_or_n} fd] } {
            ${log}::warn "Could not open $fd_or_n: $fd"
            return -code error "Could not open $fd_or_n: $fd"
        }
    } else {
        set fd $fd_or_n
    }
    
    # Read from <fd> all the indices matching the allowance and
    # restriction patterns.  Account for which indices are read.
    set dumped [list]
    upvar $ary_p OBJECT
    while { ! [eof $fd] } {
        set line [string trim [gets $fd]]
        if { $line eq "" } {
            if { $fd eq $fd_or_n } {
                ${log}::info "Spontaneously stopping to read on empty line"
                break
            }
        } else {
            set firstchar [string index $line 0]
            # Skip all lines that are commented.
            if { [string first $firstchar $UOBJ(comments)] < 0 } {
                foreach {idx val} $line {}
                set dump 1
                foreach ptn $pterns {
                    if { [string match $ptn $idx] } {
                        foreach r $rstrict {
                            if { [string match $r $idx] } {
                                set dump 0
                            }
                        }
                    }
                }
                
                if { $dump } {
                    set OBJECT($idx) $val
                    lappend dumped $idx
                }
            }
        }
    }
    
    # Close the file if the second parameter was a file name.
    if { $fd ne $fd_or_n } {
        close $fd
    }
    
    return $dumped
}


proc ::uobj::readoptions { ary_p fd_or_n {allowedkeys ""} } {
    variable UOBJ
    variable log
    
    # Guess if second argument is an opened file descriptor or a file
    # name.  If it is a file name, open it.  Always make sure fd is a
    # file descriptor to which we will write.
    if { [catch {fconfigure $fd_or_n}] } {
        ${log}::info "Reading options from $fd_or_n"
        if { [catch {open $fd_or_n} fd] } {
            ${log}::warn "Could not open $fd_or_n: $fd"
            return -code error "Could not open $fd_or_n: $fd"
        }
    } else {
        set fd $fd_or_n
    }
    
    upvar $ary_p OBJECT
    if { $allowedkeys eq "" } {
        set allowedkeys [array names OBJECT "-*"]
    }
    
    set lineno 0
    set readkeys [list]
    while { ! [eof $fd] } {
        set l [gets $fd]
        incr lineno;  # Keep track of line numbers for error spotting.
        if { $l ne "" } {
            set first [string index $l 0]
            if { [string first $first $UOBJ(comments)] < 0 } {
                set l [string trim $l]
                if { [llength $l] < 2 } {
		    ${log}::warn "Invalid line \#$lineno (\"$l\") in\
                            $fd_or_n"
                } else {
                    set key [lindex $l 0]
                    if {[string index $key 0] ne "-"} {
                        set key -$key
                    }
                    set val [lindex $l 1]
                    
                    # Scream gently when forbidden options/keys are
                    # read, otherwise set the value into the array.
                    set allowed 0
                    foreach allowedkey $allowedkeys {
                        if { [string match $allowedkey $key] } {
                            set allowed 1
                            break
                        }
                    }
                    if { $allowed } {
                        set OBJECT($key) $val
                        lappend readkeys $key
                    } else {
			${log}::warn "Key $key is invalid in $fd_or_n \
                                      should match one of\
                                [join $allowedkeys ", "]"
                    }
                }
            }
        }
    }
    # Close the file if the second parameter was a file name.
    if { $fd ne $fd_or_n } {
        close $fd
    }
    
    # Return the valid keys that were read.
    return $readkeys
}



if { [llength [info commands tailcall]] } {
    proc ::uobj::__dispatch { obj ns method args } {
        if { [string match \[a-z\] [string index $method 0]] } {
            tailcall ${ns}::${method} $obj {*}$args
        } else {
            return -code error "$method is internal to $ns!"
        }
    }
    
    proc ::uobj::__rdispatch { obj ns methods method args } {
        set ns [string trimright ${ns} :]
        foreach meths $methods {
            if { [lsearch $meths $method] >=0 } {
                # Look for all possible candidates since we want to be able
                # to support aliases.
                foreach candidate $meths {
                    # Support aliasing into another command. If meths contained
                    # {{call sensor} sensor} the code that follows arranges to
                    # recognise a command called sensor, which leads to calling
                    # call sensor followed by the arguments passed to the
                    # dispatcher.
                    set remaining [lassign $candidate cmd]; # lassign returns what wasn't assigned!
                    if { [info commands ${ns}::$cmd] ne "" } {
                        # If we've found the implementation of the command
                        # execute it, don't forget to pass additional internal
                        # (implementation) options before the args from the
                        # caller.
                        tailcall __dispatch $obj $ns $cmd {*}$remaining {*}$args
                    }
                }
                return -code error "Cannot find any implementation for $method in $ns!"
            }
        }
        return -code error "$method is not allowed in $obj!"
    }
} else {
    proc ::uobj::__dispatch { obj ns method args } {
        if { [string match \[a-z\] [string index $method 0]] } {
            return [uplevel 1 [linsert $args 0 ${ns}::${method} $obj]]
        } else {
            return -code error "$method is internal to $ns!"
        }
    }
    
    proc ::uobj::__rdispatch { obj ns methods method args } {
        set ns [string trimright ${ns} :]
        foreach meths $methods {
            if { [lsearch $meths $method] >=0 } {
                # Look for all possible candidates since we want to be able
                # to support aliases.
                foreach candidate $meths {
                    set cmd [lindex $candidate 0]
                    if { [info commands ${ns}::$cmd] ne "" } {
                        set remaining [lrange $candidate 1 end]
                        set call [linsert $remaining 0 [namespace current]::__dispatch $obj $ns $cmd]
                        foreach a $args {
                            lappend call $a
                        }
                        # If we've found the implementation of the command
                        # execute it
                        return [uplevel 1 $call]
                    }
                }
                return -code error "Cannot find any implementation for $method in $ns!"
            }
        }
        return -code error "$method is not allowed in $obj!"
    }
}


proc ::uobj::objectify { o commands } {
    interp alias {} $o {} \
            [namespace current]::__rdispatch $o [namespace qualifiers $o] $commands
}

proc ::uobj::objectify:old { o commands } {
    proc $o { cmd args } [string map [list @o@ $o @c@ $commands] {
        set passed 0
        foreach cgrp {@c@} {
            foreach c $cgrp {
                if { $cmd eq [lindex $c 0] } {
                    set passed 1
                    return [eval [namespace current]::[lindex $cgrp 0] \
                            @o@ [lrange $c 1 end] $args]
                }
            }
        }
        
        if { ! $passed } {
	    return -code error "$cmd is not a command recognised by @o@,\
                    accepted commands are [join {@c@} ", "]"
        }
    }]
}


# ::uobj::isa -- Test type of object.
#
#       This procedure test the existence and type of an object.
#
# Arguments:
#       o	Object identifier as returned by new
#       types	List of types that the object should be
#
# Results:
#       Return 1 if the object exists in its namespace and is of one
#       of the types listed, 0 otherwise.
#
# Side Effects:
#       None.
proc ::uobj::isa { o { types "" } } {
    variable log
    
    if { [info vars $o] eq "" } {
        ${log}::warn "$o is not an existing object"
        ${log}::debug [stacktrace]
        return 0
    }
    
    set type [type $o]
    if { [llength $types] > 0 && [lsearch $types $type] < 0 } {
	${log}::warn "$o is an object of type $type, none of\
                [join $types ", "]"
        ${log}::debug [stacktrace]
        return 0
    }
    
    return 1
}



# ::uobj::allof -- Return all object of given type
#
#       This procedure will return all objects, in a given namespace,
#       of a given type.
#
# Arguments:
#       ns	Namespace to create identifier in
#       type	Type of the object.
#       within	List of ids (as in incremented int) of container object
#
# Results:
#       Return the list of all objects matching the type in the namespace.
#
# Side Effects:
#       None.
proc ::uobj::allof { ns type { within "" } } {
    variable log
    
    set ns [string trim $ns ":"]
    if { $within eq "" } {
        set ptn "::${ns}::${type}_*"
    } else {
        set ptn "::${ns}::${type}_[join $within _]_*"
    }
    return [info vars $ptn]
}


# ::uobj::find -- Find an object by key comparison
#
#       This procedure will return all matching objects, in a given
#       namespace, of a given type.  All matching specifications need
#       to be true for the object to be returned.  The procedure
#       currently supposes that the object is implemented as an array.
#       Match specifications are triplets represented by the name of
#       they key in the array, the comparison operator and the
#       value. For example {name eq foobar} would include the object
#       if its key "name" was equal to "foobar".
#
# Arguments:
#       ns	Namespace to create identifier in
#       type	Type of the object.
#       matches	3-ary list where each triplet specify matching rules as above.
#       within	List of ids (as in incremented int) of container object
#
# Results:
#       Return the list of matching objects.
#
# Side Effects:
#       None.
proc ::uobj::find { ns type matches { within "" } } {
    variable log
    
    set objects [list]
    foreach o [::uobj::allof $ns $type $within] {
        upvar \#0 $o OBJ
        
        set match 1
        foreach { key cmp val } $matches {
            if { [array names OBJ $key] ne "" } {
                # Compare the value of the key to the value passed in
                # the matching triplets.  Note that we do not brace
                # around the expression on purpose to be able to
                # expand properly the comparison operator.  This also
                # forces us to add the quotes, which MIGHT be a bad
                # idea, but I couldn't find any other solution...
                if { [catch {expr \"$OBJ($key)\" $cmp \"$val\"} res] } {
		    ${log}::warn "Could not compare $key $cmp $val in\
                            object $o: $res!"
                    set match 0
                } else {
                    if { $res == 0 } {
                        set match 0
                        break;  # Stop at first matching failure
                    }
                }
            } else {
                ${log}::warn "$key is not defined in object $o"
                set match 0
            }
        }
        if { $match } {
            lappend objects $o
        }
    }
    
    return $objects
}


proc ::uobj::__generate { ns type { within "" } { id "" } } {
    variable IDGENE
    variable log
    
    set ns [string trim $ns ":"]
    if { $id eq "" } {
        if { [array names IDGENE $ns] eq "" } {
            ${log}::debug "Initialising ID generation for namespace $ns"
            set IDGENE($ns) 0
            set id 0
        } else {
            set id [incr IDGENE($ns)]
        }
    } else {
        set id [string trim $id]
        if { ![string is alnum $id] } {
	    ${log}::error "Forced identifier $id is not alpha-numerical,\
                    are you sure?!"
            return -code error "Forced identifier $id has to be alpha-numerical"
        }
    }
    
    if { $within eq "" } {
        set o "::${ns}::${type}_$id"
    } else {
        set o "::${ns}::${type}_[join $within _]_$id"
    }
    return [string map [list :::: ::] $o]
}


# ::uobj::new -- Create object identifier
#
#       This procedure creates the identifier for a pseudo object to
#       be created in the namespace passed as a parameter.  The
#       identifier contains the type of the object, which might be
#       created "within" another object, i.e. within the context of
#       the id of another object.  Uniqueness of identifiers is
#       guaranteed through the usage of an id generator which is
#       specific to each namespace.
#
# Arguments:
#       ns	Namespace to create identifier in
#       type	Type of the object.
#       within	List of ids (as in incremented int) of container object
#       id	Force object identifier, ensure they are UNIQUE within the ns!
#
# Results:
#       Return a fully qualified name that can be used in the caller
#       namespace.
#
# Side Effects:
#       None.
proc ::uobj::new { ns type { within "" } { id "" } } {
    variable IDGENE
    variable log
    
    if { $id eq "" } {
        # Generate a new identifier until we find a free one.
        set o [__generate $ns $type $within]
        while { [info exists $o] } {
            set o [__generate $ns $type $within]
        }
        ${log}::debug "Creating object $o"
    } else {
        # Forced ID, accept the way that things are.
        set o [__generate $ns $type $within $id]
        if { [info exists $o] eq "" } {
            ${log}::debug "Creating object $o"
        }
    }
    return $o
}



# ::uobj::id -- Id of object
#
#       This procedure returns the automatically generated unique id
#       of an object, i.e. the integer that gets incremented for every
#       object that is created in the new procedure.
#
# Arguments:
#       o	Full name of the object.
#
# Results:
#       Returns the id of an object, or empty string.
#
# Side Effects:
#       None.
proc ::uobj::id { o } {
    set uscore [string last "_" $o]
    if { $uscore < 0 } {
        return ""
    }
    
    incr uscore
    return [string range $o $uscore end]
}



# ::uobj::delete -- Delete an object
#
#       This is a helper procedure that deletes objects that would
#       have been created via this module.  The procedure cleans up
#       both the array that would contain the object, but also any
#       procedure that would have the name of the object (as a result
#       of objectify) and all keywords that have been associated to
#       the object.
#
# Arguments:
#       o	Full name of the object.
#
# Results:
#       None.
#
# Side Effects:
#       Upon deletion, all internal data for the object is LOST!
proc ::uobj::delete { o } {
    variable UOBJ
    variable log
    variable RESOLVER
    
    # Remove variable.
    if { [info exists $o] } {
        unset $o
    }
    
    # Remove procedure
    if { [info procs $o] ne "" } {
        rename $o ""
    }
    
    # Remove all keywords associations (which makes GC slightly unecessary)
    if { [info exists RESOLVER($o)] } {
        unset RESOLVER($o)
    }
    
    ${log}::debug "Removed object $o from interpreter context"
}


# ::uobj::type -- Return the type of an object
#
#	This procedure returns the type of one object.  The type is
#	taken from the identifier of the object itself.  This
#	procedure follows the identifiers that are created by the new
#	procedure.
#
# Arguments:
#	o	Identifier of the object.
#
# Results:
#	Return type of object or an empty string.
#
# Side Effects:
#	None.
proc ::uobj::type { o } {
    # Find last available ":" in name, this is where the namespace
    # part of the object name ends.
    set colon [string last ":" $o]
    if { $colon < 0 } {
        return ""
    }
    incr colon; # Advance past the ":" character
    
    # Now find the first underscore, jump over the 2 first characters
    # so as to also recognise temporary object types, i.e. the ones
    # beginning with two underscores.
    set uscore [string first "_" $o [expr $colon + 2]]
    if { $uscore < 0 } {
        return ""
    }
    
    return [string range $o $colon [expr $uscore - 1]]
}



# ::uobj::__resolv_gc -- Garbage collect keyword associations.
#
#       Remove all associations that would have been made for an
#       object that does not exist anymore.  Existence of "objects" is
#       just a test on the existence of the variable (which should be
#       encapsulated in a namespace).  Garbage collection only occurs
#       once out of XX calls to this function (which is controlled by
#       the resolv_gc_period global value) and can be turned off when
#       the global value is negative.
#
# Results:
#       The number of objects for which associations were removed and
#       forgotten.
#
# Side Effects:
#       None.
proc ::uobj::__resolv_gc {} {
    variable UOBJ
    variable log
    variable RESOLVER
    
    set nb_cleared 0
    if { $UOBJ(resolv_gc_period) > 0 } {
        incr UOBJ(resolv_access)
        if { [expr {$UOBJ(resolv_access) % $UOBJ(resolv_gc_period)}] == 0 } {
            foreach o [array names RESOLVER] {
                if { ![info exists $o] } {
		    ${log}::debug "Removing all resolving associations for\
                            object $o"
                    unset RESOLVER($o)
                    incr nb_cleared
                }
            }
        }
    }
    
    return $nb_cleared
}


# ::uobj::keyword -- Associate keywords to objects for disk resolution
#
#       This procedure takes care of the declaration of key/value
#       associations for objects that can be used when performing disk
#       resolution (see resolve procedure).
#
# Arguments:
#       o	Identifier of the object (as returned by ::uobj::new)
#       key	Name of the key (any string with or withour surrounding % signs)
#       value	Value for the key within the context of that object.
#
# Results:
#      The prior value of key associated to that object, which when no
#      value is given to the key is the current value, if any.
#
# Side Effects:
#       Garbage collect old associations, i.e. associations that are
#       within the context of objects that do not exist anymore
#       (i.e. variables that have disappeared).
proc ::uobj::keyword { o key {value "--dO_Not|Set=Value--"}} {
    variable UOBJ
    variable log
    variable RESOLVER
    
    __resolv_gc
    
    # Access all key values associations
    if { [info exists RESOLVER($o)] } {
        array set ASSOCS $RESOLVER($o)
    } else {
        array set ASSOCS [list]
    }
    
    # Get rid of % signs around the key and remember current value, if
    # any.
    set key [string trim $key "%"]
    if { [array names ASSOCS $key] ne "" } {
        set oldval $ASSOCS($key)
    } else {
        set oldval ""
    }
    
    # Associate value to key, if the value is not the "impossible"
    # value.  We do this in that weird way to allow to set values to
    # an empty string.
    if { $value ne "--dO_Not|Set=Value--" } {
        set ASSOCS($key) $value
	${log}::debug "Associated $key->$value for object $o for future\
                resolutions"
        
        set RESOLVER($o) [array get ASSOCS]
    }
    
    return $oldval
}


# ::uobj::resolve -- Superset of the ::diskutil::fname_resolv
#
#       This procedure provides a super-set of the facilities offered
#       by ::diskutil::fname_resolv in that it offers the ability to
#       use the values of the keys that have been associated to the
#       object via the ::uobj::keyword procedure.  All declared keys
#       that are surrounded by % signs in the incoming path will be
#       replaced by their value at run-time.
#
# Arguments:
#       o		Identifier of the object (as returned by ::uobj::new)
#       path		Path to resolved
#       override	List of pairs to override current key/pairs values
#
# Results:
#       Return a resolved path, i.e. the result of
#       ::diskutil::fname_resolv combined by the resolution of all
#       object-specific keywords.
#
# Side Effects:
#       Garbage collect old associations, i.e. associations that are
#       within the context of objects that do not exist anymore
#       (i.e. variables that have disappeared).
proc ::uobj::resolve { o path { override {} } } {
    variable UOBJ
    variable log
    variable RESOLVER
    
    __resolv_gc
    set fname [::diskutil::fname_resolv $path]
    if { [info exists RESOLVER($o)] } {
        array set LR $RESOLVER($o)
        array set LR $override
        foreach k [array names LR] {
            set fname [regsub -all "%${k}%" $fname $LR($k)]
        }
    }
    if { $path ne "" && $path ne $fname } {
        ${log}::debug "Resolved '$path' to '$fname'"
    }
    
    return $fname
}


package provide uobj 0.5

