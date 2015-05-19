# store.tcl --
#
#	This module implements a glorified <key,value> storage
#	structure.  These are in practice implemented as tcl arrays,
#	but the package provides for permanent storage in the form of
#	file, for parameter modification notification and for
#	reading/writing these to files.
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

package require Tcl 8.2
package require logger
package require struct::list

package provide param::store 1.0


namespace eval ::param::store {
    # Variables of name ::param::store::_info_<id> and
    # ::param::store::_value_<id> are created as arrays to support
    # each storage structure

    # Initialise global state
    variable STORE
    if { ![info exists STORE]} {
	array set STORE {
	    id_gene    0
	    stores     ""
	    loglevel   warn
	    comments   "\# ; !"
	}
	variable log [::logger::init ::param::store]
	${log}::setlevel $STORE(loglevel)
    }


    namespace export new
    namespace export read
    namespace export write
}

# ::param::store::loglevel -- Set/Get current log level.
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
proc ::param::store::loglevel { { loglvl "" } } {
    variable STORE
    variable log

    if { $loglvl != "" } {
	if { [catch "${log}::setlevel $loglvl"] == 0 } {
	    ::set STORE(loglevel) $loglvl
	}
    }

    return $STORE(loglevel)
}


# ::param::store::__list_read -- Read content of list file
#
#	Read a list-like value from a file
#
# Arguments:
#	fname	Name of file from which parameters should be read.
#
# Results:
#	Returns the linearised content of the file.
#
# Side Effects:
#	None
proc ::param::store::__list_read { fname } {
    variable log
    variable STORE

    
    ${log}::notice "Reading list from: $fname"
    if { [catch "open $fname r" lf] != 0 } {
	${log}::warn "Unable to open $fname"
	return {}
    }

    ::set l ""
    while { ! [eof $lf] } {
	::set line [string trim [gets $lf]]
	if { $line != "" } {
	    ::set firstchar [string index $line 0]
	    if { [lsearch $STORE(comments) $firstchar] < 0 } {
		${log}::debug "Adding $line to list content"
		if { [llength $line] == 2 } {
		    ::set value [lindex $line 1]
		    ::set firstvchar [string index $value 0]
		    if { $firstvchar == "@" } {
			::set fname [string range $value 1 end]
			::set value [::param::store::__list_read $fname]
			lappend l [list [lindex $line 0] $value]
		    } else {
			lappend l $line
		    }
		} else {
		    lappend l $line
		}
	    }
	}
    }
    close $lf

    return $l
}


# ::param::store::read -- Read content of file into store
#
#	Read a parameter file.  The content is appended to the
#	table. Any existing value will be erased.
#
# Arguments:
#	store	Identifier of store, as return by ::param::store::new
#	fname	Name of file from which parameters should be read.
#
# Results:
#	Returns the number of parameter values read, -1 on error.
#
# Side Effects:
#	Read values from file, override any existing value with same
#	name in store.
proc ::param::store::read { store fname } {
    variable log
    variable STORE

    upvar \#0 $store Store
    
    upvar \#0 $Store(storevar) Values

    ${log}::notice "Reading parameter DB from: $fname"
    if { [catch "open $fname r" pf] != 0 } {
	${log}::warn "Unable to open $fname"
	return -1
    }

    ::set nb_entries 0
    while { ! [eof $pf] } {
	::set line [string trim [gets $pf]]
	if { $line != "" } {
	    ::set firstchar [string index $line 0]
	    if { [lsearch $STORE(comments) $firstchar] < 0 } {
		::set key [lindex $line 0]
		::set val [concat [lrange $line 1 end]]
		incr nb_entries \
		    [llength [::param::store::set $store $key $val]]
	    }
	}
    }
    close $pf

    return $nb_entries
}



# ::param::store::write -- Write (sub) content of store into file
#
#	Write the content of a parameter table to a file.  Possibly
#	restricting ourselves to some of the indices.
#
# Arguments:
#	store	Identifier of store, as return by ::param::store::new
#	fname	Name of file from which parameters should be read.
#	indices	Restrictive list of index names to dump, empty for all.
#
# Results:
#	Returns the number of parameter values written, -1 on error.
#
# Side Effects:
#	Actively write to a file on the disk.
proc ::param::store::write { store fname { indices "" } } {
    variable log
    variable STORE

    upvar \#0 $store Store
    upvar \#0 $Store(storevar) Values

    ${log}::notice "Write parameter DB to: $fname"
    if { [catch "open $fname w" pf] != 0 } {
	${log}::warn "Unable to open $fname for writing"
	return -1
    }

    ::set nb_entries 0
    if { $indices == "" } {
	::set indices [array names Values]
    }
    foreach idx $indices {
	puts $pf "$idx \{$Values($idx)\}"
	incr nb_entries
    }
    close $pf

    return $nb_entries
}


# ::param::store::multiset -- Set a number of parameters
#
#	Set a number of parameters and notifies if necessary.
#
# Arguments:
#	store	Identifier of store, as return by ::param::store::new
#	p_list	List of pairs param value
#
# Results:
#	Return the list of parameters that were set.
#
# Side Effects:
#       For all permanent parameters, the file will be written
proc ::param::store::multiset { store p_list } {
    variable log
    variable STORE

    upvar \#0 $store Store
    upvar \#0 $Store(storevar) table

    ::set len [llength $p_list]
    ::set i 0
    ::set params_set ""
    while { $i < $len } {
	# Get parameter and its new value from the incoming list.
	::set param [lindex $p_list $i]
	incr i
	::set value [lindex $p_list $i]
	incr i

	# Check whether value is a reference to a list file, read it if it is.
	::set firstvchar [string index $value 0]
	if { $firstvchar == "@" } {
	    ::set fname [string range $value 1 end]
	    ::set value [::param::store::__list_read $fname]
	}

	# Now set the parameter.
	${log}::debug "Setting $param = $value"
	::set table($param) "$value"
	lappend params_set $param

	# And tell all interested parties.
	::set interest [::param::store::__get_interested $store $param]
	foreach cb $interest {
	    eval $cb SET $store $param
	}
    }

    return $params_set
}


# ::param::store::set -- Set a single parameter
#
#	Set a parameter to a given value
#
# Arguments:
#	store	Identifier of store, as return by ::param::store::new
#	key	Name of parameter
#	value	New value of parameter
#
# Results:
#	Return the parameter if it was set, an empty string otherwise
#
# Side Effects:
#       If the parameter is permanent, the storage file will be written
proc ::param::store::set { store key value } {
    return [::param::store::multiset $store [list $key $value]]
}


# ::param::store::unset -- Unset a number of parameters
#
#	Unset a number of parameters and notifies if necessary.
#
# Arguments:
#	store	Identifier of store, as return by ::param::store::new
#	p_list	List of pairs param value
#
# Results:
#	Return the list of parameters that were unset.
#
# Side Effects:
#       For all permanent parameters, the file will be written
proc ::param::store::unset { store p_list } {
    variable log
    variable STORE

    upvar \#0 $store Store
    upvar \#0 $Store(storevar) table

    ::set params_unset ""
    foreach param $p_list {
	if { [array names table -exact $param] != "" } {
	    # Now unset the parameter.
	    ${log}::debug "Unsetting $param"
	    ::unset table($param)
	    lappend params_unset $param

	    # And tell all interested parties.
	    ::set interest [::param::store::__get_interested $store $param]
	    foreach cb $interest {
		eval $cb UNSET $store $param
	    }
	}
    }

    return $params_unset
}



# ::param::store::__storecb -- Handle read/write to permanent storage
#
#	Arranged as an internal set/unset callback, this command
#	handles read and write to the permanent storage for permanent
#	parameters.
#
# Arguments:
#	type	Type of operation (SET or UNSET)
#	store	Identifier of store, as return by ::param::store::new
#	param	Name of parameter
#
# Results:
#	None.
#
# Side Effects:
#       For all permanent parameters, the file will be written
proc ::param::store::__storecb { type store param } {
    variable log
    variable STORE

    upvar \#0 $store Store
    ${log}::debug "Storage callback: $type $store $param"

    if { $Store(auto_save) } {
	if { $type == "SET" } {
	    if { [lsearch -exact $Store(permanent_indices) $param] >= 0 } {
		::param::store::write $store \
		    $Store(permanent_fname) $Store(permanent_indices)
	    }
	} elseif { $type == "UNSET" } {
	    ::param::store::__unpermanent $store $param
	}
    }
}


# ::param::store::multistore -- Permanently store a number of parameters
#
#	Permanently store a number of parameters and notifies if
#	necessary.
#
# Arguments:
#	store	Identifier of store, as return by ::param::store::new
#	p_list	List of pairs param value
#
# Results:
#	Return the list of parameters that were stored.
#
# Side Effects:
#       For all permanent parameters, the file will be written
proc ::param::store::multistore { store p_list } {
    variable log
    variable STORE

    upvar \#0 $store Store

    # Add the new keys to those known as being permanent.
    ::set old_permanent $Store(permanent_indices)
    for { ::set i 0 } { $i < [llength $p_list] } { incr i 2 } {
	lappend Store(permanent_indices) [lindex $p_list $i]
    }
    ::set Store(permanent_indices) [lsort -unique $Store(permanent_indices)]

    # See to register callbacks for the new keys, the callbacks will
    # take care of writing to the file.
    foreach key $Store(permanent_indices) {
	if { [lsearch -exact $old_permanent $key] < 0 } {
	    ::param::store::addwatch $store $key ::param::store::__storecb
	}
    }
    
    # Now we can simply set the param list, the callbacks will take
    # care of the rest.
    ::set Store(auto_save) 0
    ::set stored [::param::store::multiset $store $p_list]
    ::param::store::write $store \
	$Store(permanent_fname) $Store(permanent_indices)
    ::set Store(auto_save) 1
}


# ::param::store::store -- Permanently store a parameter
#
#	Permanently store a parameter and notifies if necessary
#
# Arguments:
#	store	Identifier of store, as return by ::param::store::new
#	key	Name of parameter
#	value	New value of parameter
#
# Results:
#	Return the parameter if it was set, an empty string otherwise
#
# Side Effects:
#       The paramter will be marked as permanently stored, so the file
#       will be written each time it is changed.
proc ::param::store::store { store key value } {
    return [::param::store::multistore $store [list $key $value]]
}


# ::param::store::__unpermanent -- Make a number of parameters non permanent
#
#	Make a number of parameters not permanent anymore
#
# Arguments:
#	store	Identifier of store, as return by ::param::store::new
#	p_list	List of parameters
#
# Results:
#	None
#
# Side Effects:
#       The storage file will be written and possibly removed as a
#       result.
proc ::param::store::__unpermanent { store unstored } {
    variable log
    variable STORE

    ${log}::debug "Non-permanentising $unstored"
    upvar \#0 $store Store

    # Compute new list of permanent indices
    ::set new_perms ""
    foreach key $Store(permanent_indices) {
	if { [lsearch -exact $unstored $key] < 0 } {
	    lappend new_perms $key
	}
    }
    ::set Store(permanent_indices) $new_perms

    # Remove callbacks on the permanent indices that were unstored.
    foreach key $unstored {
	::param::store::delwatch $store $key ::param::store::__storecb
    }

    if { [llength $Store(permanent_indices)] == 0 } {
	file delete -force -- $Store(permanent_fname)
    } else {
	::param::store::write $store \
	    $Store(permanent_fname) $Store(permanent_indices)
    }
}


# ::param::store::unstore -- Unstore a number of parameters
#
#	Unstore a number of parameters and notifies if necessary, the
#	parameters will automatically be unset.
#
# Arguments:
#	store	Identifier of store, as return by ::param::store::new
#	p_list	List of pairs param value
#
# Results:
#	Return the list of parameters that were unset.
#
# Side Effects:
#       The storage file will be written and possibly removed as a
#       result.
proc ::param::store::unstore { store p_list } {
    variable log
    variable STORE

    ${log}::debug "Unstoring $p_list"
    upvar \#0 $store Store

    ::set Store(auto_save) 0
    ::set unstored [::param::store::unset $store $p_list]
    if { $unstored != "" } {
	::param::store::__unpermanent $store $unstored
    }
    ::set Store(auto_save) 1

    return $unstored
}



# ::param::store::multiget -- Get one or several parameters
#
#	Get the value of several parameters
#
# Arguments:
#	store	Identifier of store, as return by ::param::store::new
#	p_list	List of parameters name patterns.
#
# Results:
#	A list ready for an array set command will be returned.
#
# Side Effects:
#	None.
proc ::param::store::multiget { store p_list } {
    variable log
    variable STORE

    upvar \#0 $store Store
    upvar \#0 $Store(storevar) table

    foreach ptn $p_list {
	array set result [array get table $ptn]
    }
    
    if { [info vars result] != "" } {
	return [array get result]
    } else {
	return {}
    }
}


# ::param::store::get -- Get the value of one parameter
#
#	Get the value of one parameter
#
# Arguments:
#	store	Identifier of store, as return by ::param::store::new
#	param	Name of parameter
#
# Results:
#	The current value, or an empty string.
#
# Side Effects:
#	None.
proc ::param::store::get { store param } {
    variable log
    variable STORE

    upvar \#0 $store Store
    upvar \#0 $Store(storevar) table

    if { [array names table $param] != "" } {
	return $table($param)
    } else {
	return ""
    }
}


# ::param::store::__get_interested -- Get interested parties
#
#	Get the callback that have been expressed for a given
#	parameter.
#
# Arguments:
#	store	Identifier of store, as return by ::param::store::new
#	param	Name of parameter
#
# Results:
#	Returns the list of interested callbacks, possibly empty.
#
# Side Effects:
#	None.
proc ::param::store::__get_interested { store param } {
    variable log
    variable STORE

    upvar \#0 $store Store

    ::set interest ""
    foreach w_info $Store(watchs) {
	::set ptn [lindex $w_info 0]
	::set cb [lindex $w_info 1]
	
	if { [string match $ptn $param] } {
	    lappend interest $cb
	}
    }

    return [lsort -unique $interest]
}


# ::param::store::addwatch -- Start watching for matching parameters
#
#	Register a procedure which will be called everytime a
#	parameter which names matches the argument is set or unset.
#
# Arguments:
#	store	Identifier of store, as return by ::param::store::new
#	ptn	New matching pattern.
#	cb      procedure to be called back. first argument will be
#	        operation type (SET, UNSET), second will be identifier
#               of store, last the key.
#
# Results:
#	None
#
# Side Effects:
#       None.
proc ::param::store::addwatch { store ptn cb } {
    variable log
    variable STORE

    upvar \#0 $store Store
    lappend Store(watchs) [list $ptn $cb]
}


# ::param::store::delwatch -- Stop watching for matching parameters
#
#	Remove an existing watch installed through
#	::param::store::delwatch.
#
# Arguments:
#	store	Identifier of store, as return by ::param::store::new
#	ptn	Matching pattern.
#	cb      procedure to be called back.
#
# Results:
#	true if the watch was removed, false otherwise
#
# Side Effects:
#       None.
proc ::param::store::delwatch { store ptn cb } {
    variable log
    variable STORE

    upvar \#0 $store Store
    for { ::set i 0 } { $i < [llength $Store(watchs)] } { incr i } {
	::set w_info [lindex $Store(watchs) $i]
	if { [lindex $w_info 0] == $ptn && [lindex $w_info 1] == $cb } {
	    ::set Store(watchs) [lreplace $Store(watchs) $i $i]
	    return 1
	}
    }

    return 0
}


# ::param::store::new -- Create a new store
#
#	Create a new storage structure, possibly initialising its
#	content from a permanent storage file.
#
# Arguments:
#	storedb	Name of file that will contain the permanent storage
#
# Results:
#	Return an identifier for the store that will be used in all
#	further calls to the library.
#
# Side Effects:
#	Read (and will actively write later) from (to) a file.
proc ::param::store::new { { storedb "" } } {
    variable log
    variable STORE
    
    # Register a new store
    incr STORE(id_gene)
    ::set varname "::param::store::_info_$STORE(id_gene)"
    upvar \#0 $varname Store

    # Remember information about the store and initialise it.
    lappend STORE(stores) $varname
    ::set Store(permanent_fname) $storedb
    ::set Store(permanent_indices) ""
    ::set Store(id) $STORE(id_gene)
    ::set Store(storevar) "::param::store::_values_$STORE(id_gene)"
    ::set Store(infovar) $varname
    ::set Store(watchs) ""
    ::set Store(auto_save) 1

    if { $Store(permanent_fname) != "" \
	     && [file exists $Store(permanent_fname)] } {
	::param::store::read $Store(infovar) $Store(permanent_fname)
	::set Store(permanent_indices) [array names $Store(storevar)]
    }

    return $Store(infovar)
}
