# tax.tcl --
#
#	This module implements a simplistic XML parser in 10 lines of
#	code.  It is mostly suitable for small files, but has
#	successfully been tested on larger files.  The code is based
#	heavily on Stephen Uhler's HTML parser in 10 lines, modified
#	by Eric Kemp-Benedict for XML (see http://wiki.tcl.tk/14534)
#	and revisited to offer a number of additional features for the
#	TIL.  This module also offers a generic facility for
#	(re)outputting XML code that has been parsed.
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

package require Tcl 8.4
package require logger

package require uobj
package provide tax 1.0

namespace eval ::tax {
    # Initialise the global state
    variable TAX
    if {![::info exists TAX]} {
	array set TAX {
	    extra_debug   0
	    idgene        0
	    xmlout        ""
	    -indent       "  "
	    -file         ""
	    -variable     ""
	    -autoclean    on
	    -autotrim     on
	}
	::uobj::install_log tax TAX; # Creates 'log' namespace variable
	::uobj::install_defaults tax TAX
    }
    namespace export new parse output
}


# ::tax::__cleanprops -- Clean parsed XML properties
#
#	This command cleans parsed XML properties by removing the
#	trailing slash and replacing equals by spaces so as to produce
#	a list that is suitable for an array set command.
#
# Arguments:
#	props	Parsed XML properties
#
# Results:
#	Return an event list that is suitable for an array set
#
# Side Effects:
#	None.
proc ::tax::__cleanprops { props } {
    set name {([A-Za-z_:]|[^\x00-\x7F])([A-Za-z0-9_:.-]|[^\x00-\x7F])*}
    set attval {"[^"]*"|'[^']*'|\w}; # "... Makes emacs happy
    return [regsub -all -- "($name)\\s*=\\s*($attval)" \
		[regsub "/$" $props ""] "\\1 \\4"]
}


# ::tax::parse -- Low-level 10 lines magic parser
#
#	This procedure is the core of the tiny XML parser and does its
#	job in 10 lines of "hairy" code.  The command will call the
#	command passed as an argument for each XML tag that is found
#	in the XML code passed as an argument.  Error checking is less
#	than minimum!  The command will be called with the following
#	respective arguments: name of the tag, boolean telling whether
#	it is a closing tag or not, boolean telling whether it is a
#	self-closing tag or not, list of property (array set-style)
#	and body of tag, if available.
#
# Arguments:
#	cmd	Command to call for each tag found.
#	xml	String containing the XML to be parsed.
#	start	Name of the pseudo tag marking the beginning/ending of document
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::tax::parse {cmd xml {start docstart}} {
    regsub -all \{ $xml {\&ob;} xml
    regsub -all \} $xml {\&cb;} xml
    set exp {<(/?)([^\s/>]+)\s*([^>]*)>}
    set sub "\}\n$cmd {\\2} \[expr \{{\\1} ne \"\"\}\] \[regexp \{/$\} {\\3}\] \
            \[regsub -all -- \{\\s+|(\\s*=\\s*)\} \
            \[regsub \{/$\} {\\3} \"\"\] \" \"\] \{"
    regsub -all $exp $xml $sub xml
    eval "$cmd {$start} 0 0 {} \{$xml\}"
    eval "$cmd {$start} 1 0 {} {}"
}
proc ::tax::parse {cmd xml {start docstart}} {
    regsub -all \{ $xml {\&ob;} xml
    regsub -all \} $xml {\&cb;} xml
    set exp {<(/?)([^\s/>]+)\s*([^>]*)>}
    set sub "\}\n$cmd {\\2} \[expr \{{\\1} ne \"\"\}\] \[regexp \{/$\} {\\3}\] \
             \[::tax::__cleanprops \{\\3\}\] \{"
    regsub -all $exp $xml $sub xml
    eval "$cmd {$start} 0 0 {} \{$xml\}"
    eval "$cmd {$start} 1 0 {} {}"
}


# ::tax::parse -- Callback relay to account for calling tag tree.
#
#	This internal procedure complies to the callback arguments of
#	::tax::parse.  Its purpose is to keep track of the tag calling
#	tree and merge the open/close arguments into one.
#
# Arguments:
#	id	Identifier of context
#	cmd	Command to call for each tag found.
#	tag	XML tag being opened/closed
#	cl	Boolean, true if tag is closing
#	selfcl	Boolean, true if tag is self-closing (like <br />)
#	props	List of tag properties, it any. array set-style.
#	bdy	Body text of tag.
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc tax::__callbacker {id cmd tag cl selfcl props bdy} {
    variable TAX
    variable log

    # Get to context for parsing
    set varname "::tax::cx_${id}"
    upvar \#0 $varname CONTEXT

    # Merge closing and self-closing information into a type where O
    # means open, C means close and both characters might be present
    # in type.  Also, keeps track of calling tag tree in context.
    set tagpath $CONTEXT(lvl)
    if { $selfcl } {
	if { $TAX(extra_debug) } {
	    ${log}::debug "Self-closing tag: $tag"
	}
	set type "OC"
    } elseif { $cl } {
	set CONTEXT(lvl) [lrange $CONTEXT(lvl) 0 end-1]
	set tagpath $CONTEXT(lvl)
	if { $TAX(extra_debug) } {
	    ${log}::debug "Poping tag: $tag"
	}
	set type "C"
    } else {
	if { [string index $tag 0] ne "?" } {
	    lappend CONTEXT(lvl) $tag
	}
	if { $TAX(extra_debug) } {
	    ${log}::debug "Pushing tag: $tag"
	}
	set type "O"
    }
    
    # Relay the command with the new callback syntax.
    eval "$cmd $tag $type \{$props\} \{$bdy\} \{$tagpath\}"

    # Automatically remove and clean-up the context once we have
    # finished to parsed the document.
    if { [string first "C" $type] >= 0 && [llength $CONTEXT(lvl)] == 0 } {
	${log}::debug "Final tag, auto-removing context"
	unset CONTEXT
    }
}


# ::tax::new -- Return a new high-level tax command
#
#	This procedure is relays the old ::tax::parse and the new
#	(this library) stype of callback arguments.  It returns the
#	name of an internal command that complies to the old parsing
#	callback argument format and will callback the command passed
#	as an argument with the new callback argument format.  In
#	short, the command passed as an argument to ::tax::new will be
#	called with the following arguments during parsing: name of
#	tag, type of tag (see below), list of properties, body of tag
#	and list of all previous tag in the XML tag.  The type is a
#	string composed of one or more of the "O" and "C" character.
#	"O" means tag opening, "C" means tag closing.
#
# Arguments:
#	cmd	Command to call for each tag found.
#
# Results:
#	An internal command to relay between the argument formats.
#
# Side Effects:
#	None.
proc ::tax::new {cmd} {
    variable TAX

    # Create a new context
    set id [incr TAX(idgene)]
    set varname "::tax::cx_${id}"
    upvar \#0 $varname CONTEXT
    set CONTEXT(id) $id
    set CONTEXT(lvl) ""

    # And return a command that takes the identifier of the context as
    # an argument so as to be able to modify it under parsing.
    return "::tax::__callbacker $id \"$cmd\""
}


# ::tax::output -- Dumps back an XML file
#
#	This procedure dumps back to the standard output the content
#	of an XML file.  It complies to the new callback argument
#	structure.
#
# Arguments:
#	tag	XML tag being parsed.
#	type	Type of tag: "O" for open "C" for close
#	props	List of properties.
#	bdy	Body of tag
#	tree	List of tag preceeding tag in the XML tree.
#
# Results:
#	None.
#
# Side Effects:
#	Dump on stdout.
proc ::tax::output { tag type props bdy tree } {
    variable TAX

    if { [xmlout:find stdout off] eq "" } {
	xmlout:new stdout -file stdout
    }
    xmlout:out stdout $tag $type $props $bdy $tree
}


# ::tax::xmlout:new -- New context for XML output
#
#	This procedure creates a new context for XML output.  The
#	arguments are a number of dash-led options, possibly preceeded
#	by an identifier.  When such an identifier is given, it is
#	supposed to be an external (unique) identifier that further
#	calls can use.  This identifier cannot start with a dash.  See
#	xmlout:config for a list of known options.
#
# Arguments:
#	args	Dash-led options, possibly preceeded by an identifier
#
# Results:
#	Return the internal (unique) identifier for the output
#	context, this identifier can be used in all further calls to
#	the output facility.
#
# Side Effects:
#	Can open files for output if told so (see arguments).
proc ::tax::xmlout:new { args } {
    variable TAX
    variable log

    # Recognise the initial argument as an identifier (or not) and
    # register newly created contexts.
    set create 0
    if { [string match "-*" [lindex $args 0]] || [llength $args] == 0 } {
	# No identifier at the beginning of the arguments, create a
	# new context without further logic.
	set xo [namespace current]::xmlout_[incr TAX(idgene)]
	set id $xo
	lappend TAX(xmlout) $xo $id
	set create 1
    } else {
	# The first argument is an identifier, revert to the existing
	# context if we have one, otherwise create a new one.
	set id [lindex $args 0]
	set args [lrange $args 1 end]
	set xo [xmlout:find $id off]
	if { $xo eq "" } {
	    set xo [namespace current]::xmlout_[incr TAX(idgene)]
	    lappend TAX(xmlout) $xo $id
	    set create 1
	}
    }

    # (re)initialise the (new) context.
    upvar #0 $xo XOUT
    set XOUT(id) $id
    set XOUT(varname) $xo
    set XOUT(indent) 0
    if { $create } {
	set XOUT(fd) ""
	::uobj::inherit TAX XOUT
    }

    # Configure the context and return its internal identifier
    eval xmlout:config $xo $args
    return $xo
}


# ::tax::xmlout:find -- Find an XML output context
#
#	This procedure looks for an XML output context among those
#	that we know of.  Lookup can be done on the various sorts of
#	identifier for the contexts.
#
# Arguments:
#	id	Identifier of context to look for.
#	internalalso	Should we look for internal identifiers also?
#
# Results:
#	Return the internal identifier of the XML output context if
#	found, an empty string otherwise.
#
# Side Effects:
#	None.
proc ::tax::xmlout:find { id { internalalso on} } {
    variable TAX
    variable log

    foreach {x i} $TAX(xmlout) {
	if { $id eq $i || ([string is true $internalalso] && $id eq $x)} {
	    return $x
	}
    }
    return ""
}


# ::tax::xmlout:config -- Configure an XML output
#
#	This procedure gets or sets the options of an XML output
#	context.  The options that are currently being recognised are
#	the following.  -file is the path to a file that should be
#	opened for output or a file descriptor to which XML code
#	should be output.  -variable is the name of a variable that
#	will contain the formatted XML output.  -indent is a string
#	composed of spaces that should be used for identation,
#	e.g. two spaces for two space indentation.  -autoclean tells
#	the output routine to automatically delete the context (and
#	close the file) when the whole document has been parsed.
#
# Arguments:
#	out	Internal or external identifier of XML output
#	argx	List of key values when setting, one key or none when getting
#
# Results:
#	This procedure will either set or get the options associated
#	to an XML output context.  When called with no arguments it
#	returns a list with all the options and their values.  When
#	called with one argument it returns the value of that option.
#	Otherwise, it sets the options passed in the arguments
#	together with their values.
#
# Side Effects:
#	Will attempt to open a file as output when -file is specified
#	and is not an already open file descriptor.
proc ::tax::xmlout:config { out args } {
    variable TAX
    variable log

    set xo [xmlout:find $out]
    if { $xo eq "" } {
	${log}::warn "$out is not a known XML output"
	return -code error "$out is not a known XML output"
    }
    upvar #0 $xo XOUT

    set result [eval ::uobj::config XOUT "-*" $args]

    # Ensure that we handle both file names and opened file
    # descriptors for -file.  We guess by calling fconfigure on the
    # -file argument, which should fail if the argument is not a file
    # descriptor.
    if { $XOUT(-file) ne "" } {
	if { [catch {fconfigure $XOUT(-file)}] } {
	    ${log}::info "Opening $XOUT(-file) for XML output"
	    if { [catch {open $XOUT(-file) w} fd] } {
		${log}::warn "Could not open $XOUT(-file) for XML output: $fd!"
		return -code error "Could not open $XOUT(-file) for XML output!"
	    }
	    set XOUT(fd) $fd
	} else {
	    set XOUT(fd) $XOUT(-file)
	}
    }

    # Initialise the reception variable to an empty string if it does
    # not exist.
    if { $XOUT(-variable) ne "" } {
	if { [catch {set $XOUT(-variable)}] } {
	    ${log}::info "Initialising output variable $XOUT(-variable)"
	    set $XOUT(-variable) ""
	}
    }

    return $result
}


# ::tax::xmlout:get -- Get XML output information
#
#	This procedure returns some semi-internal XML output
#	properties to other modules.  The properties that are
#	recognised are 'internal' (the internal identifier),
#	'external' (the external identifier), 'fd' the file descriptor
#	opened for output, 'indent' the level of indentation within
#	the file, or any other option of the shader (all starting with
#	a dash (-)).
#
# Arguments:
#	out	Internal or external identifier of XML output
#	type	Property to get
#
# Results:
#	The value of the property
#
# Side Effects:
#	None.
proc ::tax::xmlout:get { out type } {
    variable TAX
    variable log

    set xo [xmlout:find $out]
    if { $xo eq "" } {
	${log}::warn "$out is not a known XML output"
	return -code error "$out is not a known XML output"
    }
    upvar #0 $xo XOUT

    switch -glob -- $type {
	"fd" -
	"indent" {
	    return $XOUT($type)
	}
	"internal" {
	    return $XOUT(varname)
	}
	"external" {
	    return $XOUT(id)
	}
	"-*" {
	    return [xmlout:config $out $type]
	}
    }

    return ""
}


# ::tax::xmlout:indent -- Set/Get Indentation level
#
#	This procedure will set or get the indentation level to use
#	within the file.
#
# Arguments:
#	out	Internal or external identifier of XML output
#	indent	New indentation level to use starting from now.
#
# Results:
#	Return the current indentation level, possibly after modification
#
# Side Effects:
#	None.
proc ::tax::xmlout:indent { out {indent ""}} {
    variable TAX
    variable log

    set xo [xmlout:find $out]
    if { $xo eq "" } {
	${log}::warn "$out is not a known XML output"
	return -code error "$out is not a known XML output"
    }
    upvar #0 $xo XOUT

    if { $indent ne "" && [string is integer $indent] && $indent >= 0 } {
	set XOUT(indent) $indent
    }

    return $XOUT(indent)
}


# ::tax::xmlout:__append -- Append a string to an output
#
#	This procedure appends the result of parsing to an existing
#	XML output context.  The string is appended to the file and/or
#	string variable that are associated to the XML context.
#	Newline characters in the incoming string are used to detect
#	how to output to the file whenever relevant.
#
# Arguments:
#	xo	Internal identifier of an XML output
#	str	String to append
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::tax::xmlout:__append { xo str } {
    variable TAX
    variable log

    upvar #0 $xo XOUT

    # Append to the file if the XML output is configured that way.  If
    # there is not any end of line at the end of the string the string
    # is simply appended to the file with -nonewline.
    if { $XOUT(fd) ne "" } {
	if { [string index $str end] eq "\n" } {
	    puts $XOUT(fd) [string range $str 0 end-1]
	} else {
	    puts -nonewline $XOUT(fd) $str
	}
    }

    # Append to the text variable is the XML output is configured that
    # way.
    if { $XOUT(-variable) ne "" } {
	append $XOUT(-variable) $str
    }
}


# ::tax::xmlout:tag -- Output one XML tag
#
#	This procedure will output an XML tag with properties at the
#	current indentation level and current position in an XML
#	output.  The tag will be opened, with its properties, its body
#	written and then close in the output context.  This can be
#	used to insert additional tags as parsing goes on.
#
# Arguments:
#	id	Internal or external identifier of the XML output context
#	tag	XML tag to output
#	bdy	Body of XML tag
#	props	List of properties for the tag.
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::tax::xmlout:tag { id tag {bdy "" } {props {}} } {
    variable TAX
    variable log

    # Find output context and trigger error if not found.
    set xo [xmlout:find $id]
    if { $xo eq "" } {
	${log}::warn "$out is not a known XML output"
	return -code error "$out is not a known XML output"
    }
    upvar #0 $xo XOUT
    
    ${log}::debug "Output tag: $tag"
    
    set indent [string repeat $XOUT(-indent) $XOUT(indent)]
    if { [string is true $XOUT(-autotrim)] } {
	set tag [string trim $tag]
	set bdy [string trim $bdy]
    }
    xmlout:__append $xo "${indent}<$tag"
    if { [llength $props] >= 0 } {
	foreach {k v} $props {
	    if { [string is true $XOUT(-autotrim)] } {
		set k [string trim $k]
		set v [string trim $v]
	    }
	    xmlout:__append $xo " $k=\"$v\""
	}
    }

    if { $bdy eq "" } {
        xmlout:__append $xo " />\n"
    } else {
	xmlout:__append $xo ">$bdy</$tag>\n"
    }
}


# ::tax::xmlout:hiertag -- Output one hierarchical XML tag
#
#	This procedure will output an opening or closing hierarchical
#	XML tag with properties at the current indentation level and
#	current position in an XML output.  The current indentation
#	level will be modified to reflect tag insertion.  This can be
#	used to insert additional tags as parsing goes on.
#
# Arguments:
#	id	Internal or external identifier of the XML output context
#	tag	XML tag to output
#	opening	Is this an opening tag (otherwise will be closing)
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::tax::xmlout:hiertag { id tag {opening on}} {
    variable TAX
    variable log

    # Find output context and trigger error if not found.
    set xo [xmlout:find $id]
    if { $xo eq "" } {
	${log}::warn "$out is not a known XML output"
	return -code error "$out is not a known XML output"
    }
    upvar #0 $xo XOUT
    
    ${log}::debug "Output hierarchical tag: $tag"

    if { [string is true $XOUT(-autotrim)] } {
	set tag [string trim $tag]
    }
    if { [string is true $opening] } {
	set indent [string repeat $XOUT(-indent) $XOUT(indent)]
	xmlout:__append $xo "${indent}<$tag>\n"
	incr XOUT(indent)
    } else {
	incr XOUT(indent) -1
	set indent [string repeat $XOUT(-indent) $XOUT(indent)]
	xmlout:__append $xo "${indent}</$tag>\n"
    }
}


# ::tax::xmlout:out -- Output back XML code
#
#	This procedure is designed to output back parsed XML code as a
#	callback of the tax parser.  The procedure will do the
#	necessary formatting and output, as specified in the XML
#	output context.
#
# Arguments:
#	id	Internal or external identifier of the XML output context
#	tag	XML tag being parsed.
#	type	Type of tag: "O" for open "C" for close
#	props	List of properties.
#	bdy	Body of tag
#	tree	List of tag preceeding tag in the XML tree.
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::tax::xmlout:out { id tag type props bdy tree } {
    variable TAX
    variable log

    # Find output context and trigger error if not found.
    set xo [xmlout:find $id]
    if { $xo eq "" } {
	${log}::warn "$out is not a known XML output"
	return -code error "$out is not a known XML output"
    }
    upvar #0 $xo XOUT

    ${log}::debug "Output back XML code for $tag"

    if { [string is true $XOUT(-autotrim)] } {
	set tag [string trim $tag]
	set bdy [string trim $bdy]
    }

    # Get rid of XML header information, just duplicate it
    if { [string index $tag 0] eq "?" } {
	xmlout:__append $xo "<$tag"
	foreach {k v} [string range $props 0 end-1] {
	    if { [string is true $XOUT(-autotrim)] } {
		set k [string trim $k]
		set v [string trim $v]
	    }
	    xmlout:__append $xo " $k=\"$v\""
	}
	xmlout:__append $xo "?>\n"
	return
    }

    # The first tag is a false "docstart" tag, ignore it on opening,
    # do the necessary cleanup on closing.  Note that the following
    # code will work whichever tag is used since it only reason around
    # the size of the tree.
    if { [llength $tree] == 0 } {
	if { $type == "C" } {
	    # Cleanup on document end, either by removing the whole
	    # context or by simply closing the file that was opened if
	    # appropriate.
	    if { [string is true $XOUT(-autoclean)] } {
		xmlout:delete $xo
	    } else {
		# Close the file if we had opened it ourselves, in
		# which case the -file option (a file name) is not
		# equal to the file descriptor used for output.
		if { $XOUT(fd) ne "" && $XOUT(-file) ne $XOUT(fd) } {
		    close $XOUT(fd)
		    set XOUT(fd) ""
		}
	    }
	}
	return
    }


    # Reformat and output parsed XML code appropriately
    if { $type == "C" } {
	if { $XOUT(hadbody) } {
	    xmlout:__append $xo "</$tag>\n"
	} else {
	    incr XOUT(indent) -1
	    set indent [string repeat $XOUT(-indent) $XOUT(indent)]
	    xmlout:__append $xo "${indent}</$tag>\n"
	}
	set XOUT(hadbody) 0
    } else {
	set indent [string repeat $XOUT(-indent) $XOUT(indent)]
	xmlout:__append $xo "${indent}<$tag"
	if { [llength $props] >= 0 } {
	    foreach {k v} $props {
		if { [string is true $XOUT(-autotrim)] } {
		    set k [string trim $k]
		    set v [string trim $v]
		}
		xmlout:__append $xo " $k=\"$v\""
	    }
	}
	if { $type == "O" } {
	    if { [string trim $bdy] eq "" } {
		set XOUT(hadbody) 0
		incr XOUT(indent)
		xmlout:__append $xo ">\n"
	    } else {
		set XOUT(hadbody) 1
		xmlout:__append $xo ">$bdy"
	    }
	} else {
	    xmlout:__append $xo " />\n"
	}
    }
}


# ::tax::xmlout:delete -- Delete an XML output context
#
#	This procedure removes an XML output context, closing its
#	associated file if necessary.  Any reference to the context
#	will is lost.
#
# Arguments:
#	id	Internal or external identifier of the XML output context
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::tax::xmlout:delete { id } {
    variable TAX
    variable log

    set xo [xmlout:find $id]
    if { $xo eq "" } {
	${log}::warn "$out is not a known XML output"
	return -code error "$out is not a known XML output"
    }
    upvar #0 $xo XOUT
    
    if { $XOUT(fd) ne "" && $XOUT(-file) ne $XOUT(fd) } {
	close $XOUT(fd)
	set XOUT(fd) ""
    }

    # Look for the internal identifier in the list of known context
    # and remove the pair internal/external identifier.  We have
    # constructed the list so that the internal identifier is always
    # before the external identifier, so we know that the index is the
    # one of the internal identifier even when the internal and the
    # external identifiers are the same.
    set idx [lsearch $TAX(xmlout) $xo]
    if { $idx >= 0 } {
	set TAX(xmlout) [lreplace $TAX(xmlout) $idx [expr $idx + 1]]
    }

    unset XOUT
}
