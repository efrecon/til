# We require at least 8.4, anything below is just too old by our standards.
package require Tcl 8.4
package require logger

namespace eval ::compatibility {
    variable COMPAT
    if { ! [info exists COMPAT] } {
        array set COMPAT {
            loglevel         warn
            checked          0
        }
        variable log [::logger::init [string trimleft [namespace current] ::]]
        variable libdir [file dirname [file normalize [info script]]]
        ${log}::setlevel $COMPAT(loglevel)
    }
    namespace export loglevel check
}


# ::compatibility::loglevel -- Set/Get current log level.
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
proc ::compatibility::loglevel { { loglvl "" } } {
    variable COMPAT
    variable log
    
    if { $loglvl != "" } {
        if { [catch "${log}::setlevel $loglvl"] == 0 } {
            set COMPAT(loglevel) $loglvl
        }
    }
    
    return $COMPAT(loglevel)
}


# ::compatibility::lassign -- Assign variables from values in list
#
#	This is an implementation of lassign for older version of tcl, see
#	https://wiki.tcl.tk/1530#pagetocaa2ba245
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
#	sets variables
proc ::compatibility::lassign {values args} {
    uplevel 1 [list foreach $args [linsert $values end {}] break]
    lrange $values [llength $args] end
}


# ::compatibility::lmap -- Iterate over all elements in one or more lists and collect results
#
#	This is an implementation of lmap for older version of tcl, it makes the
#	difference between versions with {*} construct available. Comes from
#	https://wiki.tcl.tk/40570
#
# Arguments:
#	args	see manual
#
# Results:
#	None.
#
# Side Effects:
#	None.
if { [package vcompare [package provide Tcl] 8.5] < 0 } {
    proc ::compatibility::lmap args {
        set body [lindex $args end]
        set args [lrange $args 0 end-1]
        set n 0
        set pairs [list]
        foreach {varnames listval} $args {
            set varlist [list]
            foreach varname $varnames {
                upvar 1 $varname var$n
                lappend varlist var$n
                incr n
            }
            lappend pairs $varlist $listval
        }
        set temp [list]
        eval foreach $pairs [list {
            lappend temp [uplevel 1 $body]
        }]
        set temp
    }
} else {
    proc ::compatibility::lmap args {
        set body [lindex $args end]
        set args [lrange $args 0 end-1]
        set n 0
        set pairs [list]
        foreach {varnames listval} $args {
            set varlist [list]
            foreach varname $varnames {
                upvar 1 $varname var$n
                lappend varlist var$n
                incr n
            }
            lappend pairs $varlist $listval
        }
        set temp [list]
        foreach {*}$pairs {
            lappend temp [uplevel 1 $body]
        }
        set temp
    }
}

# ::compatibility::lreverse -- Iterate over all elements in one or more lists and collect results
#
#	This is an implementation of lmap for older version of tcl, it makes the
#	difference between versions with {*} construct available. Comes from
#	https://wiki.tcl.tk/40570
#
# Arguments:
#	args	see manual
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::compatibility::lreverse {list} {
    set res {}
    set i [llength $list]
    while {$i} {
        lappend res [lindex $list [incr i -1]]
    }
    set res
}


# ::compatibility::lset -- Places a value into a position in a list.
#
#	This is an implementation of lset for older version of tcl where it could
#	not add to lists. The implementation supposes that the original lset should
#	have been renamed into a command called ::compatibility::lset_orig.  Comes
#	from https://wiki.tcl.tk/9220#pagetocff9fa84c
#
# Arguments:
#	lname	Name of list variable.
#	args	see manual
#
# Results:
#	None.
#
# Side Effects:
#	None.
if { [package vcompare [package provide Tcl] 8.5] < 0 } {
    proc ::compatibility::lset {lname args} {
        upvar 1 $lname list
        # > Decode listified or not listified arguments
        if {2 == [llength $args]} {
            lassign $args indexlist newvalue
        } else {
            set newvalue [lindex $args end]
            set indexlist [lrange $args 0 end-1]
        }
        if {0 < [llength $indexlist]} {
            # > Check end index
            set endindex [lindex $indexlist end]
            set indexpath [lrange $indexlist 0 end-1]
            set sublist [eval [linsert $indexpath 0 lindex $list]]
            # substitute end in endindex
            set endindex [string map [list end [expr {[llength $sublist]-1}]]\
                    $endindex]
            # evaluate any "+"
            set endindex [expr $endindex]
            if {$endindex == [llength $sublist]} {
                lappend sublist $newvalue
                lset list $indexpath $sublist
                return $list
            }
        }
        return [eval linsert $args 0 lset_ori list]
    }
} else {
    proc ::compatibility::lset {lname args} {
        upvar 1 $lname list
        # > Decode listified or not listified arguments
        if {2 == [llength $args]} {
            lassign $args indexlist newvalue
        } else {
            set newvalue [lindex $args end]
            set indexlist [lrange $args 0 end-1]
        }
        if {0 < [llength $indexlist]} {
            # > Check end index
            set endindex [lindex $indexlist end]
            set indexpath [lrange $indexlist 0 end-1]
            set sublist [lindex $list {*}$indexpath]
            # substitute end in endindex
            set endindex [string map [list end [expr {[llength $sublist]-1}]]\
                    $endindex]
            # evaluate any "+"
            set endindex [expr $endindex]
            if {$endindex == [llength $sublist]} {
                lappend sublist $newvalue
                lset list $indexpath $sublist
                return $list
            }
        }
        return [lset_ori list {*}$args]
    }
}


proc ::compatibility::try {args} {
    # Require at least one argument.
    if {![llength $args]} {
        throw {TCL WRONGARGS} "wrong # args: should be\
                \"try body ?handler ...? ?finally script?\""
    }

    # Scan arguments.
    set args [lassign $args body]
    set handlers {}
    while {[llength $args]} {
        set args [lassign $args type]
        switch $type {
        on {
            if {[llength $args] < 3} {
                throw {TCL OPERATION TRY ON ARGUMENT} "wrong # args to on\
                        clause: must be \"... on code variableList script\""
            }
            set args [lassign $args code variableList script]
            if {![string is integer -strict $code]} {
                if {[regexp {^[ \f\n\r\t\v]*[-+]?\d+[ \f\n\r\t\v]*$} $code]
                 || [set newCode [lsearch -exact\
                            {ok error return break continue} $code]] < 0} {
                    throw {TCL RESULT ILLEGAL_CODE} "bad completion code\
                            \"$code\": must be ok, error, return, break,\
                            continue, or an integer"
                }
                set code $newCode
            }
            lappend handlers on $code $variableList $script
        } trap {
            if {[llength $args] < 3} {
                throw {TCL OPERATION TRY TRAP ARGUMENT} "wrong # args to\
                        trap clause: must be \"... trap pattern\
                        variableList script\""
            }
            set args [lassign $args pattern variableList script]
            if {[catch {list {*}$pattern} pattern]} {
                throw {TCL OPERATION TRY TRAP EXNFORMAT} "bad prefix\
                        '$pattern': must be a list"
            }
            lappend handlers trap $pattern $variableList $script
        } finally {
            if {![llength $args]} {
                throw {TCL OPERATION TRY FINALLY ARGUMENT} "wrong # args\
                        to finally clause: must be \"... finally script\""
            }
            set args [lassign $args finally]
            if {[llength $args]} {
                throw {TCL OPERATION TRY FINALLY NONTERMINAL} "finally\
                        clause must be last"
            }
        } default {
            throw [list TCL LOOKUP INDEX {handler type} $type] "bad handler\
                    type \"$type\": must be finally, on, or trap"
        }}
    }
    if {[info exists script] && $script eq "-"} {
        throw {TCL OPERATION TRY BADFALLTHROUGH} "last non-finally clause must\
                not have a body of \"-\""
    }

    # Evaluate the script body and intercept errors.
    set code [catch {uplevel 1 $body} result options]

    # Search for and evaluate the first matching handler.
    foreach {type pattern varList script} $handlers {
        if {![info exists next] && ($type ne "on" || $pattern != $code)
         && ($type ne "trap" || ![dict exists $options -errorcode]
          || $pattern ne [lrange [dict get $options -errorcode]\
                0 [expr {[llength $pattern] - 1}]])} {
            # Skip this handler if it doesn't match.
        } elseif {$script eq "-"} {
            # If the script is "-", evaluate the next handler script that is not
            # "-", regardless of the match criteria.
            set next {}
        } else {
            # Evaluate the handler script and intercept errors.
            if {[catch {
                if {[llength $varList] >= 1} {
                    uplevel 1 [list set [lindex $varList 0] $result]
                }
                if {[llength $varList] >= 2} {
                    uplevel 1 [list set [lindex $varList 1] $options]
                }
                uplevel 1 $script
            } result newOptions] && [dict exists $newOptions -errorcode]} {
                dict set newOptions -during $options
            }
            set options $newOptions

            # Stop after evaluating the first matching handler script.
            break
        }
    }

    # Evaluate the finally clause and intercept errors.
    if {[info exists finally]
     && [catch {uplevel 1 $finally} newResult newOptions]} {
        if {[dict exists $newOptions -errorcode]} {
            dict set newOptions -during $options
        }
        set options $newOptions
        set result $newResult
    }

    # Return any errors generated by the handler scripts.
    dict incr options -level
    return {*}$options $result
}


proc ::compatibility::throw {type message} {
    if {![llength $type]} {
        return -code error -errorcode {TCL OPERATION THROW BADEXCEPTION}\
                "type must be non-empty list"
    } else {
        return -code error -errorcode $type $message
    }
}

# ::compatibility::check -- Install forward compatible implementations
#
#	Installs a number of forward compatible command implementation in pure Tcl
#	so as to catch up with 8.6+ constructs in older versions of the language.
#	The implementation uses catch to detect the presence of commands so as to
#	work if commands already have been aliased or similar.
#
# Arguments:
#	cmds	Lists of commands to check for presence and install if necessary
#
# Results:
#	Return a list of the commands names that were added in the root namespace
#
# Side Effects:
#	None.
proc ::compatibility::check { {cmds {lassign lmap lreverse lset dict throw try}} } {
    variable log

    set compat [list]
    foreach cmd $cmds {
        switch -- $cmd {
            "lassign" {
                if {[catch {lassign {}}]} {
                    ${log}::notice "Installing forward compatible version of $cmd"
                    interp alias {} ::lassign {} ::compatibility::lassign
                    lappend compat $cmd
                }
            }
            "lmap" {
                # Map a no-operation on a variable to check for command.
                if {[catch {lmap noop {} break}]} {
                    ${log}::notice "Installing forward compatible version of $cmd"
                    interp alias {} ::lmap {} ::compatibility::lmap
                    lappend compat $cmd
                }
            }
            "lreverse" {
                if {[catch {lreverse {}}]} {
                    ${log}::notice "Installing forward compatible version of $cmd"
                    interp alias {} ::lmap {} ::compatibility::lmap
                    lappend compat $cmd
                }
            }
            "lset" {
                set l {}
                if {[catch {lset l 0 end+1 3}]} {
                    ${log}::notice "Installing forward compatible version of $cmd"
                    rename ::lset ::compatibility::lset_orig
                    interp alias {} ::lset {} ::compatibility::lset
                    lappend compat $cmd
                }
            }
            "dict" {
                if {[catch {dict get {}}]} {
                    ${log}::notice "Installing forward compatible version of $cmd"
                    package require dict
                    lappend compat $cmd
                }
            }
            "throw" {
                if {[catch {throw TCL test} res] && $res ne "test" } {
                    ${log}::notice "Installing forward compatible version of $cmd"
                    # Trying package from tcllib first, otherwise internal
                    # implementation coming from wiki
                    if { [catch {package require throw}]} {
                        if { [package vcompare [package provide Tcl] 8.5] > 0 } {
                            interp alias {} ::throw {} ::compatibility::throw
                            lappend compat $cmd
                        } else {
                            ${log}::warn "No implementation of $cmd for Tcl version [package provide Tcl]"
                        }
                    } else {
                        lappend compat $cmd
                    }
                }
            }
            "try" {
                if {[catch {try {}}] } {
                    ${log}::notice "Installing forward compatible version of $cmd"
                    # Trying package from tcllib first, otherwise internal
                    # implementation coming from wiki
                    if { [catch {package require try}]} {
                        if { [package vcompare [package provide Tcl] 8.5] > 0 } {
                            interp alias {} ::try {} ::compatibility::try
                            lappend compat $cmd
                        } else {
                            ${log}::warn "No implementation of $cmd for Tcl version [package provide Tcl]"
                        }
                    } else {
                        lappend compat $cmd
                    }
                }
            }

        }
    }

    return $compat
}

# Check at once and only once, we simply want to require the package to bring
# all these in without any fluff.
if { ! $::compatibility::COMPAT(checked) } {
    ::compatibility::check
    set ::compatibility::COMPAT(checked) 1
}

package provide compatibility 1.0