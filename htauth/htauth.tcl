##################
## Module Name     --  htauth
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##    Provides basic authentication services for URLs.  This module
##    implements a dynamic database of authentication information for
##    URLs that are passed to it dynamically.  The core routine,
##    htauth::headers will return the or update headers in a format
##    that is appropriate for http::geturl calls.  The procedure will
##    either store, either automatically retrieve the authentication
##    information from/for the URL.
##
## Commands Exported:
##      ::htauth::headers
##      ::htauth::remember
##      ::htauth::obfuscate
##################

package require Tcl 8.4

package require uri
package require base64

package require uobj

namespace eval ::htauth {
    variable AUTH
    if {![info exists AUTH]} {
	array set AUTH {
	    rplc_user _
	    rplc_pwd  *
	    db        {}
	    -cleartxt 0
	    -resolver ""
	}
	variable version 0.1
	::uobj::install_log htauth AUTH
	::uobj::install_defaults htauth AUTH
    }
}


proc ::htauth::FQRoot { lst { tail "" } } {
    variable AUTH

    array set URL $lst
    # Forces HTTP at the least, should we?
    if { ![info exists URL(scheme)] } {
	set URL(scheme) http
    }
    # Force a proper default port, as of spec (not as of what we know,
    # but this is as we want it here).
    if { ![info exists URL(port)] || $URL(port) eq "" } {
	switch -nocase -- $URL(scheme) {
	    "http" -
	    "ws" { set URL(port) 80 }
	    "https" -
	    "wss" { set URL(port) 443 }
	    default {
		set URL(port) ""
	    }
	}
    }
    # Force a path and arrange to replace the tail of the path by the
    # specified tail, if any.  We perform replacement directly in the
    # path index of the URL array.
    if { ![info exists URL(path)] || $URL(path) eq "" } {
	if { $tail eq "" } {
	    set URL(path) ""
	} else {
	    set URL(path) $tail
	}
    } elseif { $tail ne "" } {
	if { [string index $URL(path) end] eq "/" } {
	    append URL(path) $tail
	} elseif { [string first "/" $URL(path)] < 0 } {
	    set URL(path) $tail
	} else {
	    set URL(path) [file dirname $URL(path)]/[string trimleft $tail "/"]
	}
    }

    # Form the URL and return it, we force the port in
    set url $URL(scheme)://$URL(host):$URL(port)/
    append url [string trimleft $URL(path) "/"]

    return $url
}


proc ::htauth::Auth { URL_p } {
    variable AUTH
    variable log

    upvar $URL_p URL
    set auth ""
    if { [string match -nocase "http*" $URL(scheme)] \
	    || [string match -nocase "ws*" $URL(scheme)] } {
	if { [info exists URL(user)] } {
	    append auth $URL(user)
	}
	append auth :
	if { [info exists URL(pwd)] } {
	    append auth $URL(pwd)
	}
	if { $auth eq ":" } {
	    set auth ""
	}
    }

    return $auth
}


# ::htauth::obfuscate -- Obfuscate away authentication information
#
#       This procedure will replace the authentication information
#       contained in a URL by information that can be presented to the
#       user or to a log file while not compromising security.  Only
#       the beginning of the username is kept, and the content of the
#       paswd is replaced by a number of stars.
#
# Arguments:
#	url	URL to obfuscate information from
#	ext	Extended mode where we extract xx:yy from the URL
#
# Results:
#       Return the obfuscated, security-enhanced, URL.
#
# Side Effects:
#       None.
proc ::htauth::obfuscate { url { ext off } } {
    variable AUTH
    variable log

    if { [string is true $AUTH(-cleartxt)] } {
	return $url
    }

    set authinfo [regexp -indices -- {//.*?:.*?@} $url range]
    if { $authinfo } {
	foreach {start end} $range break
	incr start 2
	incr end -1
    } elseif { [string is true $ext] } {
	set authinfo [regexp -indices -- {.*?\:.*?($|(?!\w))} $url range]
	if { $authinfo } {
	    foreach {start end} $range break
	}
    }

    if { $authinfo } {
	foreach {usr paswd} [split [string range $url $start $end] ":"] break
	set auth [string range $usr 0 2][string repeat $AUTH(rplc_user) 3]
	append auth ":[string repeat $AUTH(rplc_pwd) 6]"
	set header [string range $url 0 [expr {$start -1}]]
	set footer [string range $url [expr {$end + 1}] end]
	if { $footer eq "" } {
	    return ${header}${auth}
	} else {
	    return ${header}${auth}@${footer}
	}
    }

    return $url
}


# ::htauth::remember -- Remember for future use
#
#       This procedure updates our internal database with the
#       authentication information passed as a parameter.
#
# Arguments:
#	url	URL (tree) to store information for.
#	usr	Username (: separated auth. will do!)
#	paswd	Password for user to store.
#
# Results:
#       Return the URL tree for which we are storing information,
#       empty on failure.
#
# Side Effects:
#       None.
proc ::htauth::remember { url {usr ""} {paswd ""}} {
    variable AUTH
    variable log

    Split $url URL
    set auth [Auth URL]

    if { $auth eq "" } {
	set auth ${usr}:${paswd}
	if { $auth eq ":" } {
	    ${log}::warn "No authorisation hint given in [obfuscate $url]\
                          or in arguments!"
	    return ""
	}
    }

    set furl [FQRoot [array get URL] *]
    if { [lsearch -exact $AUTH(db) $furl] < 0 } {
	${log}::debug "Keeping authorisation info [obfuscate $auth 1] for all\
                       URLs matching [obfuscate $furl]"
	lappend AUTH(db) $furl $auth
    }

    return $furl
}


proc ::htauth::Split { url {url_p ""} } {
    if { $url_p ne "" } {
	upvar $url_p URL
    }
    
    if { [string match ws* $url] } {
	set surl http[string range $url 2 end]
    } else {
	set surl $url
    }
    array set URL [::uri::split $surl]
    if { $surl ne $url } {
	set URL(scheme) ws[string range $URL(scheme) 4 end]
    }
}


# ::htauth::headers -- Store/retrieve and return auth. information
#
#       When present in a URL, the authentication information for that
#       URL is stored in our internal database.  When no information
#       is present, look for the nearest match in our internal
#       database.  In either cases, return the authentication
#       information (or update the headers array) in a format that is
#       suitable for an http::geturl call.
#
# Arguments:
#	url	URL, with or without authentication information
#	hdrs_p	Pointers to an array with the headers
#
# Results:
#       Return (possibly modified) headers for passing to the
#       http::geturl interface.
#
# Side Effects:
#       None.
proc ::htauth::headers { url {hdrs_p ""} } {
    variable AUTH
    variable log

    # Access headers (for modification) and initialise if they don't
    # exist yet (to make sure the variable exists on procedure exit).
    if { $hdrs_p ne "" } {
	upvar $hdrs_p hdrs
	if { ![info exists hdrs] } {
	    set hdrs {}
	}
    } else {
	set hdrs {}
    }
    
    # Arrange for auth to be the username and password specified as
    # part of the URL.
    Split $url URL
    set auth [Auth URL]

    # If we don't have an authorisation information or if we only have
    # a username (and no password), try looking in what we already
    # have had in the past.
    if { $auth eq "" || [lindex $auth end] eq ":" } {
	${log}::debug "No authorisation specified (or no pwd)\
                       in [obfuscate $url], looking for a matching one"

	# Look for candidates in the existing database, i.e. URLs that
	# match the fully qualified root url.  We keep them in a list
	# and count the number of slashes.
	set candidates {}
	set rooturl [FQRoot [array get URL]]
	foreach {rurl ainfo} $AUTH(db) {
	    if { [string match -nocase $rurl $rooturl] } {
		lappend candidates \
		    [list $rurl $ainfo [regexp -all -indices -- / $rurl]]
	    }
	}
	
	# Sort the candidates by number of slashes, the ones that have
	# most slashes are deeper down in the hierarchy.  Loop through
	# them and stop as soon as we have found an authorisation
	# information that works.
	set afilter "${auth}*";     # leads to "*" or "username:*"
	foreach l [lsort -decreasing -integer -index 2 $candidates] {
	    foreach {rurl ainfo -} $l break
	    if { [string match $afilter $ainfo] } {
		set auth $ainfo
		${log}::debug "Extracted [obfuscate $auth 1] out of known\
                               [obfuscate $rurl] to access [obfuscate $url]"
		break
	    }
	}
	
	if { $auth eq "" || [lindex $auth end] eq ":" } {
	    ${log}::notice "No known authorisation information for\
                            [obfuscate $url]"
	}
    } else {
	# Store authorisation information for the "directory" for
	# future calls.
	set furl [FQRoot [array get URL] *]
	if { [lsearch -exact $AUTH(db) $furl] < 0 } {
	    ${log}::debug "Keeping authorisation info [obfuscate $auth 1]\
                           for all URLs matching [obfuscate $furl]"
	    lappend AUTH(db) $furl $auth
	}
    }
    
    if { $auth ne "" } {
	if { $AUTH(-resolver) ne "" } {
	    if { [catch {eval [linsert $AUTH(-resolver) end $auth]} rauth] } {
		${log}::warn "Could not resolve authorisation: $rauth"
	    } else {
		set auth $rauth
	    }
	}
	lappend hdrs Authorization "Basic [::base64::encode $auth]"
    }

    return $hdrs
}


package provide htauth $::htauth::version
