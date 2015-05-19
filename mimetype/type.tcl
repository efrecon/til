# type.tcl --
#
#	This modules provides a set of routines to actively guess the
#	MIME type of files.
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

package require Tcl 8.2
package require logger

package provide mimetype 1.0

namespace eval ::mimetype {
    # Initialise the global state
    variable MT
    if {![::info exists MT]} {
	array set MT {
	    idgene          0
	    loglevel        warn
	    read_mime_types 0
	    read_magic_map  0
	    magic_max       0
	    guessrules      ""
	}
	variable log [::logger::init mimetype]
	${log}::setlevel $MT(loglevel)
    }

    namespace export loglevel guess extensions extension
}

# ::mimetype::loglevel -- Set/Get current log level.
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
proc ::mimetype::loglevel { { loglvl "" } } {
    variable MT
    variable log

    if { $loglvl != "" } {
	if { [catch "${log}::setlevel $loglvl"] == 0 } {
	    set MT(loglevel) $loglvl
	}
    }

    return $MT(loglevel)
}


# ::mimetype::__rule_match -- Data match against rule
#
#	This command checks existing file data against a given rule.
#	This command has gone through minimal testing, and minimal
#	inspiration from the mod_mime_magic module implementation of
#	the apache web server.
#
# Arguments:
#	data	File data
#	rule	Rule specification
#
# Results:
#	Return positive if match, 0 otherwise.
#
# Side Effects:
#	None.
proc ::mimetype::__rule_match { data rule } {
    variable MT
    variable log
    global tcl_platform

    # First decide where we should start peeking data from
    set bytenumber [lindex [split $rule] 0]
    if { [string index $bytenumber 0] == ">" } {
	set bytenumber [string range $bytenumber 1 end]
    }

    # Now extract type and rule value.
    set type [string tolower [lindex $rule 1]]
    set ruleval [lindex $rule 2]
    set mtype [lindex $rule 3]
    set val ""

    # Numerical values can have an ampersand, store this value in the
    # mask to apply to the value, otherwise see to have a no-op mask.
    if { $type != "string" } {
	set idx [string first "&" $type]
	if { $idx >= 0 } {
	    incr idx
	    set mask [string range $type $idx end]
	} else {
	    set mask "0xffffffff"
	}
    }

    # Now extract the value from the data depending on its type.
    switch -glob -- $type {
	byte* {
	    catch {binary scan $data @${bytenumber}c1 val}
	    if { $val != "" } {
		set val [expr { $val & 0xff & $mask }]
	    }
	}
	short* {
	    if { $tcl_platform(byteOrder) == "littleEndian" } {
		catch {binary scan $data @${bytenumber}s1 val}
	    } elseif { $tcl_platform(byteOrder) == "bigEndian" } {
		catch {binary scan $data @${bytenumber}S1 val}
	    }
	    if { $val != "" } {
		set val [expr { $val & 0xffff & $mask }]
	    }
	}
	long* {
	    if { $tcl_platform(byteOrder) == "littleEndian" } {
		catch {binary scan $data @${bytenumber}i1 val}
	    } elseif { $tcl_platform(byteOrder) == "bigEndian" } {
		catch {binary scan $data @${bytenumber}I1 val}
	    }
	    if { $val != "" } {
		set val [expr { $val & 0xffffffff & $mask }]
	    }
	}
	string {
	    set size [__rule_size $rule]
	    catch {binary scan $data @${bytenumber}a${size} val}
	}
	date* {
	    if { $tcl_platform(byteOrder) == "littleEndian" } {
		catch {binary scan $data @${bytenumber}i1 val}
	    } elseif { $tcl_platform(byteOrder) == "bigEndian" } {
		catch {binary scan $data @${bytenumber}I1 val}
	    }
	    if { $val != "" } {
		set val [expr { $val & 0xffffffff & $mask }]
	    }
	}
	beshort* {
	    catch {binary scan $data @${bytenumber}S1 val}
	    if { $val != "" } {
		set val [expr { $val & 0xffff & $mask }]
	    }
	}
	belong* {
	    catch {binary scan $data @${bytenumber}I1 val}
	    if { $val != "" } {
		set val [expr { $val & 0xffffffff & $mask }]
	    }
	}
	bedate* {
	    catch {binary scan $data @${bytenumber}I1 val}
	    if { $val != "" } {
		set val [expr { $val & 0xffffffff & $mask }]
	    }
	}
	leshort* {
	    catch {binary scan $data @${bytenumber}s1 val}
	    if { $val != "" } {
		set val [expr { $val & 0xffff & $mask }]
	    }
	}
	lelong* {
	    catch {binary scan $data @${bytenumber}i1 val}
	    if { $val != "" } {
		set val [expr { $val & 0xffffffff & $mask }]
	    }
	}
	ledate* {
	    catch {binary scan $data @${bytenumber}i1 val}
	    if { $val != "" } {
		set val [expr { $val & 0xffffffff & $mask }]
	    }
	}
    }

    # And apply the testing rule as specified.  Most of the time it
    # will be equality!
    #${log}::debug "Testing $rule: data's value = $val"
    if { $type == "string" } {
	return [string equal $val [subst $ruleval]]
    } else {
	if { $val == "" } {
	    return 0
	} else {
	    set firstchar [string index $ruleval 0]
	    switch $firstchar {
		">" {
		    return [expr {$val > [string range $ruleval 1 end]}]
		}
		">" {
		    return [expr {$val < [string range $ruleval 1 end]}]
		}
		"!" {
		    return [expr {$val != [string range $ruleval 1 end]}]
		}
		"^" {
		    return [expr {$val ^ [string range $ruleval 1 end]}]
		}
		"=" {
		    return [expr {$val == [string range $ruleval 1 end]}]
		}
		default {
		    return [expr {$val == $ruleval}]
		}
	    }
	}
    }

    return 0; # Just to be on the safe side, should never be reached.
}


# ::mimetype::__guess_peek -- Guess the MIME type of a file
#
#	This command actively guesses the MIME type of a file through
#	reading a sufficient number of bytes and applying a number of
#	rules that resemble the file(1) command.
#
# Arguments:
#	fname	Name of file
#
# Results:
#	Return the MIME type, an empty string or an error.
#
# Side Effects:
#	None.
proc ::mimetype::__guess_peek { fname } {
    variable MT
    variable log

    # Open file and read just enough data as to be able to test all
    # currently known rules.
    if { [catch {open $fname} fdes] } {
	${log}::warn "Could not open file \"$fname\": $fdes"
	return -code error "Could not open file \"$fname\": $fdes"
    }

    fconfigure $fdes -encoding binary -translation binary
    if { [catch {read $fdes $MT(magic_max)} data] } {
	${log}::warn \
	    "Could not read $MT(magic_max) bytes from file \"$fname\": $data"
	return -code error \
	    "Could not read $MT(magic_max) bytes from file \"$fname\": $data"
    }
    catch {close $fdes}

    # Now test all rules, one after the other one.  The current
    # implementation only support one level of indirection in data
    # peeking.  My linux implementation of file seems to have rules
    # with several levels of ">", but there are none in the Apache
    # rules that I have seen.
    foreach id $MT(guessrules) {
	set varname "::mimetype::Rule_${id}"
	upvar \#0 $varname Rule

	set match [__rule_match $data [lindex $Rule(rules) 0]]
	if { $match } {
	    set len [llength $Rule(rules)]
	    if { $len > 1 } {
		for { set i 1 } { $i < $len } { incr i } {
		    if { [__rule_match $data [lindex $Rule(rules) $i]] } {
			return [lindex [lindex $Rule(rules) $i] 3]
		    }
		}
	    } else {
		return [lindex [lindex $Rule(rules) 0] 3]
	    }
	}
    }

    return ""
}


# ::mimetype::__rule_size -- Byte size of a file peek rule
#
#	This command read computes the number of bytes necessary to be
#	read from the file to decide upon the matching of a given file
#	peek rule.
#
# Arguments:
#	None
#
# Results:
#	Return the size in bytes, -1 for unknown rules.
#
# Side Effects:
#	None.
proc ::mimetype::__rule_size { rule } {
    set type [string tolower [lindex $rule 1]]
    switch -glob -- $type {
	byte* {
	    return 1
	}
	*short* {
	    return 2
	}
	*long* -
	*date* {
	    return 4
	}
	string {
	    set str [subst [lindex $rule 2]]
	    return [string length $str]
	}
    }

    return -1
}


# ::mimetype::__read_magic_map -- Read magic number mapping file
#
#	This command reads the magic number mapping files according to
#	the syntax desciption described in
#	http://httpd.apache.org/docs-2.0/mod/mod_mime_magic.html.
#	These rules will be used when deciding the MIME type of files
#	that have no extension.
#
# Arguments:
#	None
#
# Results:
#	Return 1 on success, 0 on failure.
#
# Side Effects:
#	None.
proc ::mimetype::__read_magic_map { } {
    variable MT
    variable log
    
    if { ! $MT(read_magic_map) } {
	set mm_fname [file join $::mimetype::libdir "magic.nfo"]
	
	if { [catch "open $mm_fname" fdes] == 0 } {
	    set curid -1
	    
	    while { ! [eof $fdes] } {
		set line [string trim [gets $fdes]]

		if { $line != "" } {
		    set firstchar [string index $line 0]
		    if { $firstchar != "\#" } {
			set sl [split $line]

			set bytenumber [lindex $sl 0]
			if { [string index $bytenumber 0] != ">" } {
			    set id [incr MT(idgene)]
			    set varname "::mimetype::Rule_${id}"
			    upvar \#0 $varname Rule
			    
			    set Rule(id) $id
			    set Rule(rules) ""

			    lappend MT(guessrules) $id
			    set curid $id
			}

			# Create the rule and add it to the list of
			# decision aids for the current rule.  This is
			# clumsy because split introduces empty
			# strings that we do not want.
			set varname "::mimetype::Rule_${curid}"
			upvar \#0 $varname Rule
			set rule ""
			foreach elmnt $sl {
			    if { $elmnt != "" } {
				lappend rule $elmnt
			    }
			}
			# ${log}::debug "Parsed $line -> $rule"
			lappend Rule(rules) $rule
		    }
		}
	    }
	    close $fdes

	    # Compute the maximum number of bytes necessary for all
	    # known rules.
	    set MT(magic_max) 0
	    foreach id $MT(guessrules) {
		set varname "::mimetype::Rule_${id}"
		upvar \#0 $varname Rule

		# Compute the last necessary byte for all rule
		# branches of a given rule.
		set maxbyte 0
		foreach rule $Rule(rules) {
		    set bytenumber [lindex [split $rule] 0]
		    if { [string index $bytenumber 0] == ">" } {
			set bytenumber [string range $bytenumber 1 end]
		    }
		    set rulemax [expr {$bytenumber + [__rule_size $rule]}]
		    if {$rulemax > $maxbyte } {
			set maxbyte $rulemax
		    }
		}

		# Now update the global max number of necessary bytes.
		incr maxbyte
		if { $maxbyte > $MT(magic_max) } {
		    set MT(magic_max) $maxbyte
		}
	    }

	    set MT(read_magic_map) 1
	} else {
	    ${log}::critical "Could not read MIME types info from $mm_fname"
	}
    }

    return $MT(read_magic_map)
}


# ::mimetype::__read_mime_types -- Read MIME types mapping file
#
#	This command reads the MIME types -> extensions mapping file
#	and store its content in memory for further usage.
#
# Arguments:
#	None
#
# Results:
#	Return 1 on success, 0 on failure.
#
# Side Effects:
#	None.
proc ::mimetype::__read_mime_types { } {
    variable MT
    variable log
    variable mimetype_ext
    variable ext_mimetype

    if { ! $MT(read_mime_types) } {
	set mt_fname [file join $::mimetype::libdir "mimetypes.lst"]
	
	if { [catch "open $mt_fname" fdes] == 0 } {
	    while { ! [eof $fdes] } {
		set line [string trim [gets $fdes]]

		if { $line != "" } {
		    set firstchar [string index $line 0]
		    if { $firstchar != "\#" } {
			set mimetype ""
			set extlist ""
			foreach item [split $line] {
			    if { [string trim $item] != "" } {
				if { $mimetype == "" } {
				    set mimetype [string tolower $item]
				} else {
				    lappend extlist [string tolower $item]
				}
			    }
			}
			if { [llength $extlist] > 0 } {
			    ${log}::debug "Adding $mimetype: $extlist"
			    set mimetype_ext($mimetype) $extlist
			    foreach ext $extlist {
				set ext_mimetype($ext) $mimetype
			    }
			}
		    }
		}
	    }
	    close $fdes
	    set MT(read_mime_types) 1
	} else {
	    ${log}::critical "Could not read MIME types info from $mt_fname"
	}
    }
    
    return $MT(read_mime_types)
}


# ::mimetype::extensions -- Extension list corresponding to a MIME type.
#
#	This command return the list of extensions that can be used
#	for a given MIME type.
#
# Arguments:
#	mtype	MIME type to get extensions for.
#
# Results:
#	Return the list (possibly) empty of extensions possible for
#	this MIME type.  The extensions do not contain (by
#	construction) a leading dot.
#
# Side Effects:
#	None.
proc ::mimetype::extensions { mtype } {
    variable MT
    variable log

    if { [::mimetype::__read_mime_types] } {
	variable mimetype_ext

	if { [array names mimetype_ext $mtype] != "" } {
	    return $mimetype_ext($mtype)
	}
    }

    return ""
}


# ::mimetype::__ext_len_compare -- Compare extension lengths
#
#	Compare the length of the two strings passed as a parameter.
#	This command will be called back when sorting extensions.
#
# Arguments:
#	e1	First extension
#	e2	Second extension
#
# Results:
#	Return -1 if the length of e1 is less than the length of e2,
#	+1 if e2 is longer than e1 and zero if they are of equal
#	lengths.
#
# Side Effects:
#	None.
proc ::mimetype::__ext_len_compare { e1 e2 } {
    set l1 [string length $e1]
    set l2 [string length $e2]

    if { $l1 < $l2 } {
	return -1
    } elseif { $l1 > $l2 } {
	return 1
    } else {
	return 0
    }
}


# ::mimetype::extension -- Smallest extension of a MIME type.
#
#	This command return the smallest (in length) extension that
#	corresponds to a MIME type.
#
# Arguments:
#	mtype	MIME type to get extensions for.
#
# Results:
#	Return the smallest extension for a MIME type or an empty
#	string.
#
# Side Effects:
#	None.
proc ::mimetype::extension { mimetype } {
    return [lindex [lsort -command ::mimetype::__ext_len_compare \
			[::mimetype::extensions $mimetype]] 0]
}


# ::mimetype::guess -- Guess the MIME type of a local file
#
#	This command guesses the MIME type of a local file.  This
#	command uses an algorithm that is similar to the one used by
#	the Apache Web server.  It uses the extension of the file in
#	the first place, and, when this did not lead to a working MIME
#	type, fails over to peeking bytes from the file to try
#	guessing the type, in a manner similar to the file(1)
#	implementation.
#
# Arguments:
#	fname	Name of file to analyse
#	active	If set, will immediately guess through peeking in file content
#
# Results:
#	Return the MIME type of the file, or an empty string.
#
# Side Effects:
#	This command will, if necessary, read some bytes from the file
#	at hand.
proc ::mimetype::guess { fname { active 0 } } {
    variable MT
    variable log

    # First try using the extension
    if { ! $active && [::mimetype::__read_mime_types] } {
	variable ext_mimetype

	set ext [string tolower [file extension $fname]]
	if { [string index $ext 0] == "." } {
	    set ext [string range $ext 1 end]
	}

	if { [array names ext_mimetype $ext] != "" } {
	    return $ext_mimetype($ext)
	}
    }

    # Second try using the content, in a manner similar to file(1)
    if { [::mimetype::__read_magic_map] } {
	return [__guess_peek $fname]
    }

    return ""
}
