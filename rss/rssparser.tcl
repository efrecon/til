package require Tcl
# This is a hack but should work on most platforms
if { [catch {package require xml::expat} err] } {
    package require tdom
}
package require logger

package provide rssparser 1.0

namespace eval ::rssparser {
    # Variables of name ::rssparser::Feed_<id> are created as arrays to
    # support each feed monitoring.

    # Initialise the global state
    variable RSSP
    if {![::info exists RSSP]} {
	array set RSSP {
	    idgene    0
	    parsers   ""
	    loglevel  warn
	}
	variable log [::logger::init rssparser]
	${log}::setlevel $RSSP(loglevel)
    }

    namespace export loglevel parse new destroy
}


# ::rssparser::loglevel -- Set/Get current log level.
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
proc ::rssparser::loglevel { { loglvl "" } } {
    variable RSSP
    variable log

    if { $loglvl != "" } {
	if { [catch "${log}::setlevel $loglvl"] == 0 } {
	    set RSSP(loglevel) $loglvl
	}
    }

    return $RSSP(loglevel)
}


# ::rssparser::__parse_startelement -- Start parsing of XML element
#
#	Initialises the parsing of XML elements.  Elements forming the
#	tree will be associated to arrays which data will be filled in
#	successively.  A stack is used to remember where we are within
#	the XML parsing tree.  The stack is also used to know which
#	leaf is being filled in.
#
# Arguments:
#	id	RSS parsing context.
#	name	Name of XML element
#	attlist	Attribute list
#	args	Additional arguments.
#
# Results:
#	Returns an error on failure to break the parsing routine.
#
# Side Effects:
#	None.
proc ::rssparser::__parse_startelement { id name attlist args } {
    variable RSSP
    variable log

    set idx [lsearch $RSSP(parsers) $id]
    if { $idx < 0 } {
	${log}::warn "$id is not a recognised parser object"
	return -code error "$id does not identify a parser object"
    }
    
    set varname ::rssparser::Parser_$id
    upvar \#0 $varname Parser

    switch -- $name {
	rss {
	    # RSS top element.  Create an element structure and push
	    # its identifier on the stack.
	    set i_id [incr Parser(idgene)]
	    set varname ::rssparser::Element_${id}_${i_id}
	    upvar \#0 $varname Top

	    set Top(name) $name
	    set Top(id) $i_id
	    set Top(parent) [lindex $Parser(stack) end]
	    set Top(channels) ""

	    lappend Parser(stack) $Top(id)
	    lappend Parser(elements) $Top(id)
	}
	channel {
	    # Channel. Create an element structure, initialise it and
	    # push its identifier on the stack.
	    set i_id [incr Parser(idgene)]
	    set varname ::rssparser::Element_${id}_${i_id}
	    upvar \#0 $varname Channel

	    set Channel(name) $name
	    set Channel(id) $i_id
	    set Channel(parent) [lindex $Parser(stack) end]
	    set Channel(title) ""
	    set Channel(link) ""
	    set Channel(description) ""
	    set Channel(pubDate) ""
	    set Channel(items) ""
	    set Channel(ttl) ""

	    lappend Parser(stack) $Channel(id)
	    lappend Parser(elements) $Channel(id)
	}
	item {
	    # Channel. Create an element structure, initialise it and
	    # push its identifier on the stack.
	    set i_id [incr Parser(idgene)]
	    set varname ::rssparser::Element_${id}_${i_id}
	    upvar \#0 $varname Item

	    set Item(name) $name
	    set Item(id) $i_id
	    set Item(parent) [lindex $Parser(stack) end]
	    set Item(title) ""
	    set Item(link) ""
	    set Item(description) ""
	    set Item(pubDate) ""
	    set Item(enclosure) ""
	    set Item(author) ""

	    lappend Parser(stack) $Item(id)
	    lappend Parser(elements) $Item(id)
	}
	enclosure {
	    # For enclosures, the only thing which really is
	    # interesting is the attribute list.
	    set i_id [lindex $Parser(stack) end]
	    set varname ::rssparser::Element_${id}_${i_id}
	    upvar \#0 $varname Item

	    set Item(enclosure) $attlist
	    lappend Parser(stack) $name
	}
	ttl -
	title -
	link -
	pubDate -
	author -
	description -
	default {
	    # All other elements are considered to be leaf nodes.
	    # Their name is pushed onto the stack so that
	    # __parse_characterdata will know where to put its data.
	    lappend Parser(stack) $name
	}
    }
}


# ::rssparser::__parse_endelement -- End parsing of XML element
#
#	Finishes the parsing of XML elements.  For leaf elements,
#	their name is simply poped from the stack.  For tree elements,
#	we remember their identifier in their parent to be able to
#	traverse the tree later on.
#
# Arguments:
#	id	RSS parsing context.
#	name	Name of XML element
#	args	Additional arguments.
#
# Results:
#	Returns an error on failure to break the parsing routine.
#
# Side Effects:
#	None.
proc ::rssparser::__parse_endelement { id name args } {
    variable RSSP
    variable log

    set idx [lsearch $RSSP(parsers) $id]
    if { $idx < 0 } {
	${log}::warn "$id is not a recognised parser object"
	return -code error "$id does not identify a parser object"
    }

    set varname ::rssparser::Parser_$id
    upvar \#0 $varname Parser

    set key [lindex $Parser(stack) end]
    if { [string is integer $key] } {
	set i_id $key
	set varname ::rssparser::Element_${id}_${i_id}
	upvar \#0 $varname Element
	if { $Element(name) == "item" } {
	    # Make sure we have a reference to this item in the
	    # channel containing it.
	    set c_id [lindex $Parser(stack) end-1]
	    set varname ::rssparser::Element_${id}_${c_id}
	    upvar \#0 $varname Channel

	    lappend Channel(items) $i_id
	    set Parser(stack) [lrange $Parser(stack) 0 end-1]
	} elseif { $Element(name) == "channel" } {
	    # Make sure we have a reference to this channel in the
	    # top containing it ("rss").
	    set t_id [lindex $Parser(stack) end-1]
	    set varname ::rssparser::Element_${id}_${t_id}
	    upvar \#0 $varname Top

	    lappend Top(channels) $i_id
	    set Parser(stack) [lrange $Parser(stack) 0 end-1]
	} elseif { $Element(name) == "rss" } {
	    # Do not pop the stack, we will use it from the parse command
	} else {
	    ${log}::warn "$Element(name) is an unrecognised tree builder"
	}
    } else {
	# For all leaf elements, pop from the stack simply
	set Parser(stack) [lrange $Parser(stack) 0 end-1]
    }
}


# ::rssparser::__parse_characterdata -- Parse content of element
#
#	Store the content of the element in the appropriate element
#	array.
#
# Arguments:
#	id	RSS parsing context.
#	data	Element data
#
# Results:
#	Returns an error on failure to break the parsing routine.
#
# Side Effects:
#	None.
proc ::rssparser::__parse_characterdata { id data } {
    variable RSSP
    variable log

    if { [string length $data] == "" } {
	return 
    }

    set idx [lsearch $RSSP(parsers) $id]
    if { $idx < 0 } {
	${log}::warn "$id is not a recognised parser object"
	return -code error "$id does not identify a parser object"
    }

    set varname ::rssparser::Parser_$id
    upvar \#0 $varname Parser

    set key [lindex $Parser(stack) end]
    if { ! [string is integer $key] } {
	set i_id [lindex $Parser(stack) end-1]
	set varname ::rssparser::Element_${id}_${i_id}
	upvar \#0 $varname Element

	append Element($key) [string trim $data]
    }
}


# ::rssparser::get_element -- To RSS element content.
#
#	This routine returns the name of a global array that can be
#	accessed to get the content of an element encountered during
#	parsing.  Sanity checks are performed before.

# Arguments:
#	id	Identifier of parser context
#	e_id	Identifier of element, as allocated during parsing.
#
# Results:
#	Return the variable name, ready for an "upvar #0" command or
#	an error.
#
# Side Effects:
#	None.
proc ::rssparser::get_element { id e_id } {
    variable RSSP
    variable log

    set idx [lsearch $RSSP(parsers) $id]
    if { $idx < 0 } {
	${log}::warn "$id is not a recognised parser object"
	return -code error "$id does not identify a parser object"
    }

    set varname ::rssparser::Parser_$id
    upvar \#0 $varname Parser

    set idx [lsearch $Parser(elements) $e_id]
    if { $idx < 0 } {
	${log}::warn "$e_id does not identify an element created by the parser"
	return -code error "$id does not identify an element"
    }
    
    set varname ::rssparser::Element_${id}_${e_id}
    return $varname
}


# ::rssparser::parse -- Parse RSS data
#
#	This routine parses RSS data and returns the identifier of the
#	top channel that is pointed at by the RSS data.  The name of
#	the variable holding information for that channel is returned
#	by ::rssparser::get_eleement.
#
# Arguments:
#	id	Identifier of parser context
#	data	RSS content
#
# Results:
#	Return the identifier of the top channel of the data
#
# Side Effects:
#	None.
proc ::rssparser::parse { id data } {
    variable RSSP
    variable log

    # Check that this is one of our parser contexts
    set idx [lsearch $RSSP(parsers) $id]
    if { $idx < 0 } {
	${log}::warn "$id is not a recognised parser object"
	return ""
    }

    # Get to data.
    set varname ::rssparser::Parser_$id
    upvar \#0 $varname Parser

    # Setup an XML parsing context.  We can only have one at a time
    # within a RSS context.
    set Parser(parser) [::xml::parser]
    set Parser(stack)  ""

    set Parser(parser) [::xml::parser]
    $Parser(parser) configure \
	-elementstartcommand [list ::rssparser::__parse_startelement $id] \
	-elementendcommand [list ::rssparser::__parse_endelement $id] \
	-characterdatacommand [list ::rssparser::__parse_characterdata $id]

    # Parse the XML and free all local XML structures once done.
    $Parser(parser) parse $data
    $Parser(parser) free

    # Get to the identifier of the top "rss" object, and then to its
    # direct and (only?) child, the top channel.  Return that
    # identifier.
    set t_id [lindex $Parser(stack) 0]
    set varname ::rssparser::Element_${id}_${t_id}
    upvar \#0 $varname Top

    set c_id [lindex $Top(channels) 0]

    return $c_id
}


# ::rssparser::destroy -- (Recursively) destroys RSS elements
#
#	This routine recursively destroy a tree of RSS parsing
#	elements.
#
# Arguments:
#	id	Identifier of parser context
#	topid	Top identifier of the elements to destroy.
#
# Results:
#	Return an error on failures.
#
# Side Effects:
#	None.
proc ::rssparser::destroy { id topid } {
    variable RSSP
    variable log

    set idx [lsearch $RSSP(parsers) $id]
    if { $idx < 0 } {
	${log}::warn "$id is not a recognised parser object"
	return -code error "$id does not identify a parser object"
    }

    set varname ::rssparser::Parser_$id
    upvar \#0 $varname Parser

    set idx [lsearch $Parser(elements) $topid]
    if { $idx < 0 } {
	${log}::warn \
	    "$topid does not identify an element created by the parser"
	return -code error "$id does not identify an element"
    }
    
    set varname ::rssparser::Element_${id}_${topid}
    upvar \#0 $varname Element
    
    if { $Element(name) == "channel" } {
	foreach i_id $Element(items) {
	     ::rssparser::destroy $id $i_id
	}
	set varname [::rssparser::get_element $id $Element(parent)]
	upvar \#0 $varname Top
	set p_idx [lsearch $Parser(elements) $Element(parent)]
	set Parser(elements) [lreplace $Parser(elements) $p_idx $p_idx]
	unset Top
    } elseif { $Element(name) == "item" } {
	set varname [::rssparser::get_element $id $Element(parent)]
	upvar \#0 $varname Channel
	set p_idx [lsearch $Channel(items) $topid]
	set Channel(items) [lreplace $Channel(items) $p_idx $p_idx]
    }

    unset Element
    set idx [lsearch $Parser(elements) $topid]
    set Parser(elements) [lreplace $Parser(elements) $idx $idx]
}


# ::rssparser::new -- Create new RSS parser context
#
#	Create a new RSS parser context object within which all
#	successive calls to this library will occur.  The library
#	provides contexts to allow keeping the results of several
#	parsing occurences if necessary.
#
# Arguments:
#	None
#
# Results:
#	Return an identifier for the parsing context.  This identifier
#	will be used in all further calls to the library.
#
# Side Effects:
#	None.
proc ::rssparser::new {} {
    variable RSSP
    variable log

    set id [incr RSSP(idgene)]
    set varname ::rssparser::Parser_$id
    upvar \#0 $varname Parser

    set Parser(stack)  ""
    set Parser(idgene) 0
    set Parser(elements)  ""
    set Parser(id)     $id

    lappend RSSP(parsers) $id

    return $id
}


# ::rssparser::delete -- Delete a context and all its elements.
#
#	Delete a RSS parser and all elements that have been generated
#	by one or several parses within it.
#
# Arguments:
#	id	Identifier of parser context
#
# Results:
#	None
#
# Side Effects:
#	None.
proc ::rssparser::delete { id } {
    variable RSSP
    variable log

    set idx [lsearch $RSSP(parsers) $id]
    if { $idx < 0 } {
	${log}::warn "$id is not a recognised parser object"
	return -code error "$id does not identify a parser object"
    }

    set varname ::rssparser::Parser_$id
    upvar \#0 $varname Parser

    foreach e_id $Parser(elements) {
	set varname ::rssparser::Element_${id}_${e_id}
	upvar \#0 $varname Element

	unset Element
    }
    
    set RSSP(parsers) [lreplace $RSSP(parsers) $idx $idx]
    unset Parser
}
