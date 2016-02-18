# httpd.tcl --
#
#	This modules provides a functional but minimal HTTPd
#
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.


package require Tcl 8.4
package require logger
package require uri
package require mimetype
package require diskutil
package require http
package require base64
package require html
package require sha1
package require ip
#package require tls; # We will request it on demand to make this a
#soft constraint on the HTTP package.

package provide minihttpd 1.3

package require minihttpd::dirlist
package require minihttpd::validate
package require websocket

namespace eval ::minihttpd {
    # Initialise the global state
    variable HTTPD
    if {![::info exists HTTPD]} {
	array set HTTPD {
	    loglevel         "warn"
	    default_port     8080
	    maxportallocs    20
	    servers          ""
	    dateformat       "\[%d%m%y %H:%M:%S\]"
	    validate_timeout 250
	    -default         "index.htm index.html"
	    -dirlist         "*"
	    -logfile         ""
	    -bufsize         16384
	    -sockblock       0
	    -selfvalidate    hostname
	    -externhost      ""
	    -pki             ""
	    -authorization   ""
	    -ranges          {0.0.0.0/0 ::/0}
	}
	variable log [::logger::init [string trimleft [namespace current] ::]]
	${log}::setlevel $HTTPD(loglevel)
    }
    variable HTTPD_errors
    if {![::info exists HTTPD_errors]} {
	array set HTTPD_errors {
	    204 {No Content}
	    400 {Bad Request}
	    401 {Unauthorized}
	    404 {Not Found}
	    503 {Service Unavailable}
	    504 {Service Temporarily Unavailable}
	}
    }

    namespace export loglevel new close config defaults
}


# ::minihttpd::loglevel -- Set/Get current log level.
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
proc ::minihttpd::loglevel { { loglvl "" } } {
    variable HTTPD
    variable log

    if { $loglvl != "" } {
	if { [catch "${log}::setlevel $loglvl"] == 0 } {
	    set HTTPD(loglevel) $loglvl
	}
    }

    return $HTTPD(loglevel)
}


# ::minihttpd::new -- Start HTTP serving on a port
#
#	This command will start a HTTP server on the port passed as an
#	arguments and serving files under a given root directory.
#	Connection arguments are inherited from the defaults
#	parameters and can be overriden through the additional
#	arrguments or the ::minihttpd::config call.  These are the
#	following options: -default (default file names for
#	directories) -logfile (logfile to output transactions to)
#	-bufsize (buffering size) -sockblock (blocking sockets or not)
#	-dirlist (list of patterns for which directory listing is
#	allowed when the default file does not exist (expressed in
#	server file space)) -pki followed by a list of two file path
#	(can be resolved) with a public and private key to associate
#	to the server, thus making it run over HTTPS instead of
#	HTTP. -authorization contains details for Basic authentication
#	protection.  This is a list of triplets where the first
#	element is a pattern matching a directory at the server, the
#	second element a realm and the third a list of elements where
#	the username and password of the allowed users are separated
#	by a colon.
#
# Arguments:
#	root	Root directory being served, maybe empty for REsT serving
#	port	Port on which we listen, negative to start picking at default
#               and looking for any available port.
#	args	Additional list of options and values, as described above.
#
# Results:
#	Return the port on which we listen for connections and serve
#	for HTTP, a negative number otherwise
#
# Side Effects:
#	Will give access to the files on the disk!
proc ::minihttpd::new {root port args} {
    variable HTTPD
    variable log

    # If the port passed as an argument is the one of our servers,
    # reconfigure it and return.
    if { $port > 0 } {
	set idx [lsearch $HTTPD(servers) $port]
	if { $idx >= 0 } {
	    eval config $port $args
	    return $port
	}
    }

    # Serve only if we can access the directory.
    if { $root != "" \
	     && (![file isdirectory $root] || ![file readable $root]) } {
	${log}::warn "Cannot serve $root, cannot access directory!"
	return -1
    }

    # TLS initialisation (eat some of the options in advance, this is
    # a bit strange, but will do as long as we remember that we have
    # done so.
    set proto "http"
    set socket_cmd "::socket"
    foreach {opt arg} $args {
	if { [string match -nocase -pki $opt] && $arg != "" } {
	    # Lazy require TLS package to make sure we can run the
	    # whole HTTPD server without encryption support in other
	    # cases
	    package require tls

	    # Extract public and private key files from the option
	    # -pki, these can be resolved.
	    foreach {certfile keyfile} $arg break
	    set certfile [::diskutil::fname_resolv $certfile]
	    set keyfile [::diskutil::fname_resolv $keyfile]
	    if { ! [file readable $certfile] } {
		${log}::error "Cannot access public key file: $certfile"
		return -1
	    }
	    if { ! [file readable $keyfile] } {
		${log}::error "Cannot access private key file: $certfile"
		return -1
	    }
	    # Now initialise and make sure we remember that we are
	    # running https instead of http on that port.  Support as
	    # many protocols as possible to make sure we can serve as
	    # most clients as possible, even though we know sslv2 has
	    # some vulnerability issues.
	    foreach proto {ssl2 ssl3 tls1} {
		if { [catch {::tls::ciphers $proto} ciphers] } {
		    ::tls::init -$proto 0
		    ${log}::warn "No support for $proto under HTTPS"
		} else {
		    if { [llength $ciphers] > 0 } {
			::tls::init -$proto 1
			${log}::notice "HTTPS will have support for $proto"
		    } else {
			::tls::init -$proto 0
			${log}::warn "No support for $proto under HTTPS"
		    }
		}
	    }
	    ::tls::init -certfile $certfile -keyfile $keyfile
	    set proto "https"
	    set socket_cmd ::tls::socket
	}
    }

    # Positive port will force serving there, check once and return if
    # we start listening on that port (there could be some other
    # process serving that port already).
    if { $port > 0 } {
	if { [catch {eval [list $socket_cmd \
			       -server [list ::minihttpd::__accept $port]\
			       $port]} \
		  sock] } {
	    ${log}::warn "Cannot serve on $port: $sock"
	    return -1
	}
    } else {
	# A negative port was passed, that means we should choose a
	# suitable port.  Start from the default port and increase by
	# 2 until we find an available port.  This could be an
	# infinite loop but WTH!
	set sock ""
	set port $HTTPD(default_port)
	set attempts 0
	while { $sock == "" } {
	    if { [catch {$socket_cmd \
			     -server [list ::minihttpd::__accept $port] \
			     $port} \
		      sock] } {
		set sock ""
		${log}::notice "Cannot serve on $port: $sock"
		incr port 2
		incr attempts
		if { $attempts >= $HTTPD(maxportallocs) } {
		    ${log}::warn "Attempted $HTTPD(maxportallocs) port\
                                  allocations without any success, giving up!"
		    return -1
		}
	    }
	}
    }

    set varname "::minihttpd::Server_${port}"
    upvar \#0 $varname Server

    set Server(port) $port
    if { $root != "" } {
	set Server(root) [::diskutil::absolute_path $root]
    } else {
	set Server(root) ""
    }
    set Server(hostname) ""
    set Server(ip) ""
    set Server(clients) {}
    set Server(listen) $sock
    set Server(selfvalidation_urls) {}
    set Server(handlers) {};     # External handlers for AJAX comm.
    set Server(live) "";         # WebSocket server state
    set Server(protocol) $proto
    set Server(socket_cmd) $socket_cmd
    foreach opt [array names HTTPD "-*"] {
	set Server($opt) $HTTPD($opt)
    }
    lappend HTTPD(servers) $port
    eval config $port $args

    if { $Server(root) != "" } {
	__log $port "Starting $Server(protocol) server on port \#$port,\
                     root: $Server(root)"
    } else {
	__log $port "Starting rootless $Server(protocol) server on port \#$port"
    }

    validate::init $port

    return $port
}


proc ::minihttpd::listening { port } {
    variable HTTPD
    variable log

    return [expr [lsearch $HTTPD(servers) $port] >= 0]
}


# ::minihttpd::close -- Stop HTTP serving on a port
#
#	This command will stop an existing HTTP server on the port
#	passed as an arguments.  All current connections are
#	immediately closed.
#
# Arguments:
#	port	Port number of one of our HTTP servers.
#
# Results:
#	None.
#
# Side Effects:
#	Will log on disk if required
proc ::minihttpd::close { port } {
    variable HTTPD
    variable log

    set idx [lsearch $HTTPD(servers) $port]
    if { $idx >= 0 } {
	set varname "::minihttpd::Server_${port}"
	upvar \#0 $varname Server

	catch {::close $Server(listen)}
	foreach sock $Server(clients) {
	    ::minihttpd::__disconnect $port $sock
	}
	__log $port "Stopped web server on $port"
    } else {
	${log}::warn "Not listening for HTTP connections on $port!"
    }
}


# ::minihttpd::__disconnect -- Close a socket
#
#	Close a socket that had been open as a result of a client
#	connection and cleanup all data associated to the client and
#	the socket.
#
# Arguments:
#	port	Port number of one of our HTTP servers.
#	sock	Socket to client.
#
# Results:
#	None.
#
# Side Effects:
#	Will immediately close the connection to the client, all data lost.
proc ::minihttpd::__disconnect { port sock } {
    variable HTTPD
    variable log

    set idx [lsearch $HTTPD(servers) $port]
    if { $idx >= 0 } {
	set varname "::minihttpd::Server_${port}"
	upvar \#0 $varname Server

	set idx [lsearch $Server(clients) $sock]
	if { $idx >= 0 } {
	    set varname "::minihttpd::Client_${port}_${sock}"
	    upvar \#0 $varname Client

	    fileevent $sock readable ""
	    catch {flush $sock}
	    unset Client
	    catch {::close $sock}
	    set idx [lsearch -exact $Server(clients) $sock]
	    if { $idx >= 0 } {
		set Server(clients) [lreplace $Server(clients) $idx $idx]
	    }
	} else {
	    ${log}::warn "$sock is not a recognised client of $port"
	}
    } else {
	${log}::warn "Not listening for HTTP connections on $port!"
    }
}

# ::minihttpd::__allowed -- Check IP authorisation
#
#       Check if the (incoming) IP address is allowed to connect to this server.
#       This routine considers the different ranges in CIDR notations that are
#       specified as part of the -ranges option of the server. It properly
#       differentiates between IPv6 and IPv4 addresses and automatically remove
#       any scope ID that could be present (necessary for self-hostname
#       detection).
#
# Arguments:
#	port	Port number of one of our HTTP servers
#	ipaddr	IP address to check.
#
# Results:
#       1 if client at IP address is allowed, 0 otherwise.
#
# Side Effects:
#       None.
proc ::minihttpd::__allowed { port ipaddr } {
    variable HTTPD
    variable log

    set idx [lsearch $HTTPD(servers) $port]
    if { $idx >= 0 } {
	set varname "::minihttpd::Server_${port}"
	upvar \#0 $varname Server
	
	# Check if client is allowed to connect, i.e. among one of the IP ranges
	# specified in -ranges.
	${log}::debug "Checking if client at $ipaddr is allowed"
	set ipaddr [regsub {%\w+$} $ipaddr ""];  # Remove Scope ID, see http://superuser.com/questions/99746/why-is-there-a-percent-sign-in-the-ipv6-address
	foreach range $Server(-ranges) {
	    if { [::ip::version $range] eq [::ip::version $ipaddr] } {
		set mask [::ip::mask $range]
		if { $mask eq "" } {
		    if { [::ip::equal $ipaddr $range] } {
			return 1
		    }
		} else {
		    if { [::ip::equal $ipaddr/$mask $range] } {
			return 1
		    }
		}
	    }
	}
    }
    return 0
}


# ::minihttpd::__accept -- Accept client connections
#
#	Accept connections from clients and arrange for get lines to
#	be read and treated.
#
# Arguments:
#	s_port	Port number of one of our HTTP servers.
#	sock	Socket to client.
#	ipaddr	IP Address of client
#	port	Port number to client
#
# Results:
#	None.
#
# Side Effects:
#	None
proc ::minihttpd::__accept { s_port sock ipaddr port} {
    variable HTTPD
    variable log

    set idx [lsearch $HTTPD(servers) $s_port]
    if { $idx >= 0 } {
	set varname "::minihttpd::Server_${s_port}"
	upvar \#0 $varname Server
		
	if { [__allowed $s_port $ipaddr] } {	
	    set varname "::minihttpd::Client_${s_port}_${sock}"
	    upvar \#0 $varname Client
    
	    fconfigure $sock \
		-blocking $Server(-sockblock) \
		-buffersize $Server(-bufsize) \
		-translation {auto crlf}
	    set Client(sock) $sock
	    set Client(ipaddr) $ipaddr
	    __translog $Server(port) $sock Connect $ipaddr $port
	    lappend Server(clients) $sock
	    __hostname $s_port $sock
	    fileevent $sock readable [list ::minihttpd::__pull $s_port $sock]
	} else {
	    ::close $sock
	    ${log}::warn "Incoming client at $ipaddr was rejected"
	}
    } else {
	${log}::warn "Not listening for HTTP connections on $port!"
    }
}


# ::minihttpd::__hostname -- Set/query name of host for server
#
#	Sets or simply queries the name of the host on which the
#	server currently runs.
#
# Arguments:
#	port	Port number of one of our HTTP servers.
#	sock	Socket to one existing client.
#
# Results:
#	None.
#
# Side Effects:
#	The name will be cached for further use
proc ::minihttpd::__hostname { port { sock "" } } {
    variable HTTPD
    variable log

    set idx [lsearch $HTTPD(servers) $port]
    if { $idx >= 0 } {
	set varname "::minihttpd::Server_${port}"
	upvar \#0 $varname Server

	# We have a name in the cache, return it
	if { $Server(-externhost) != "" } {
	    return $Server(-externhost)
	}
	if { $Server(hostname) != "" } {
	    return $Server(hostname)
	}
	
	# No name, check that the socket is one of ours.
	set idx [lsearch $Server(clients) $sock]
	if { $idx < 0 } {
	    set sock ""
	}

	# No socket, look we we don't have a connected client, we can
	# get information from there.
	if { $sock == "" && [llength $Server(clients)] > 0 } {
	    set sock [lindex $Server(clients) 0]
	}

	# Now, maybe we have a socket to extract information for.  If
	# we do cache the result in Server(hostname).
	if { $sock != "" } {
	    set sockinfo [fconfigure $sock -sockname]
	    set Server(hostname) [lindex $sockinfo 1]
	    if { $Server(ip) == "" || $Server(ip) == "127.0.0.1" } {
		set Server(ip) [lindex $sockinfo 0]
	    }
	}

	if { $Server(hostname) == "" } {
	    # Still in the void, return something wrong but that will
	    # be decent in most cases.
	    if { [info commands ::dnsresolv::hostname] != "" } {
		return [::dnsresolv::hostname]
	    } else {
		return [info hostname]
	    }
	} else {
	    return $Server(hostname)
	}
    } else {
	${log}::warn "Not listening for HTTP connections on $port!"
    }

    return ""
}


proc ::minihttpd::__tokenise_query { qry } {
    variable HTTPD
    variable log

    set response ""
    foreach tken [split $qry &] {
	foreach {k v} [split $tken =] break
	set val [__URLtoString $v]
	lappend response $k $val
    }
    return $response
}


proc ::minihttpd::__starve { port sock } {
    variable HTTPD
    variable log

    set idx [lsearch $HTTPD(servers) $port]
    if { $idx >= 0 } {
	set varname "::minihttpd::Server_${port}"
	upvar \#0 $varname Server
	
	set idx [lsearch $Server(clients) $sock]
	if { $idx >= 0 } {
	    set varname "::minihttpd::Client_${port}_${sock}"
	    upvar \#0 $varname Client

	    fconfigure $sock -translation binary
	    if { [eof $sock] } {
		__push $port $sock
	    }

	    if { [catch {read $sock} data] } {
		__push $port $sock
	    } else {
		append Client(data) $data
		if { [array names Client mime,content-length] != "" } {
		    set len [string length $Client(data)]
		    if { $len >= $Client(mime,content-length) } {
			__push $port $sock
		    }
		} elseif { $data eq "" } {
		    # There wasn't anything to read anymore
		    __push $port $sock
		}
	    }
	}
    }
}


# ::minihttpd::__pull -- Pull data from client
#
#	Read, understand and treat requests coming from clients.
#	Arrange for answers to be sent back.
#
# Arguments:
#	port	Port number of one of our HTTP servers.
#	sock	Socket to client.
#
# Results:
#	None.
#
# Side Effects:
#	None
proc ::minihttpd::__pull { port sock } {
    variable HTTPD
    variable log

    set idx [lsearch $HTTPD(servers) $port]
    if { $idx >= 0 } {
	set varname "::minihttpd::Server_${port}"
	upvar \#0 $varname Server
	
	set idx [lsearch $Server(clients) $sock]
	if { $idx >= 0 } {
	    set varname "::minihttpd::Client_${port}_${sock}"
	    upvar \#0 $varname Client

	    set Client(data) ""
	    if { [catch {gets $sock line} readCount] != 0 } {
		__translog $port $sock "Error" \
		    "Unable to read request line: $readCount"
		__disconnect $port $sock
	    } else {
		# Wait for a full line of input (as per the gets
		# behaviour on nonblocking I/O).  Note that this might
		# represent a security risk since sending an enormous
		# line of junk to the server might crash it.  One
		# solution would be to re-implement gets using read a
		# bind it to a given length and gracefully fail on
		# lines that are too long.
		if { $readCount < 0 && [fblocked $sock] } {
		    return
		}
		if {![info exists Client(state)]} {
		    if { [regexp {(POST|GET|HEAD) ([^?]+)\??([^ ]*) HTTP/(1.0|1.1)} \
			      $line x Client(proto) Client(url) \
			      Client(query)] } {
			set Client(state) mime
			set Client(query) [__tokenise_query $Client(query)]
			__translog $port $sock Query $line
		    } else {
			__push_error $port $sock 400 "bad first line: $line"
			return
		    }
		}
		
		set state \
		    [string compare $readCount 0],$Client(state),$Client(proto)
		switch -- $state {
		    0,mime,GET  -
		    0,mime,HEAD -
		    0,query,POST  { __push $port $sock }
		    0,mime,POST   {
			set Client(state) query
			set Client(data) ""
			fconfigure $sock -buffering none -blocking 0
			fileevent $sock readable \
			    [list ::minihttpd::__starve $port $sock]
		    }
		    1,mime,POST   -
		    1,mime,HEAD   -
		    1,mime,GET    {
			if [regexp {([^:]+):[   ]*(.*)} \
				$line dummy key value] {
			    set Client(mime,[string tolower $key]) $value
			}
		    }
		    1,query,POST  {
			append Client(data) $line "\n"
		    }
		    -1,query,POST {
			if { $Client(data) != "" } {
			    __push $port $sock
			} else {
			    fconfigure $sock -buffering none -blocking 0
			    fileevent $sock readable \
				[list ::minihttpd::__starve $port $sock]
			}
		    }
		    default {
			if { [eof $sock] } {
			    __translog $port $sock Error \
				"unexpected eof on <$Client(url)> request"
			} else {
			    __translog $port $sock Error \
				"unhandled state <$state> fetching\
                                 <$Client(url)>"
			}
			__push_error $port $sock 404 "Bad request at $state"
		    }
		}
	    }
	} else {
	    ${log}::warn "$sock is not a recognised client of $port"
	}
    } else {
	${log}::warn "Not listening for HTTP connections on $port!"
    }
}


# ::minihttpd::fullurl -- Computes full URL to local file
#
#	Return the fully qualified URL for a file relative to the root
#	if possible.  All forbidden cases will lead to empty strings.
#	This implementation is aware of rootless servers, i.e. servers
#	that only contain handlers.
#
# Arguments:
#	port	Port number of one of our HTTP servers.
#	fpath	File path (relative to root)
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::minihttpd::fullurl { port { fpath "/" } { fullpath_p "" } } {
    variable HTTPD
    variable log

    if { $fullpath_p != "" } {
	upvar $fullpath_p fullpath
    }

    set fullpath ""
    set idx [lsearch $HTTPD(servers) $port]
    if { $idx >= 0 } {
	set varname "::minihttpd::Server_${port}"
	upvar \#0 $varname Server

	# Rootless, no file can be served...
	if { $Server(root) == "" } {
	    set match 0
	    foreach {ptn cb fmt} $Server(handlers) {
		if { [string match -nocase $ptn $fpath] } {
		    set match 1
		}
	    }
	    if { ! $match } {
		set match "/"
	    }
	}

	if { [string index $fpath 0] != "/" } {
	    set fpath "/$fpath"
	}

	if { $Server(root) == "" } {
	    set mypath $fpath
	} else {
	    set mypath [::diskutil::absolute_path \
			    [__URLtoString "$Server(root)$fpath"]]
	}
	if { $Server(root) != "" } {
	    if { [string first $Server(root) $mypath] != 0 } {
		# Outside of root directory is an ERROR!
		set mypath ""
	    } else {
		if {[file isdirectory $mypath]} {
		    set defaulted 0
		    foreach d $Server(-default) {
			set fname [file join $mypath $d]
			if { [file exists $fname] && [file readable $fname] } {
			    set mypath $fname
			    set defaulted 1
			    break
			}
		    }
		    if { ! $defaulted } {
			set match 0
			foreach ptn $Server(-dirlist) {
			    if { [string match $ptn $fpath] } {
				set match 1
			    }
			}
			if { ! $match } {
			    # Generate an error, directory not allowed
			    set mypath ""
			}
		    }
		}
	    }
	}

	if { $mypath != "" } {
	    set fullpath $mypath
	    set url "$Server(protocol)://"
	    append url [__hostname $port]
	    append url ":"
	    append url $port
	    set urlpath \
		[string range $mypath [string length $Server(root)] end]
	    if { $urlpath == "" } { set urlpath "/" }
	    append url $urlpath

	    return $url
	} else {
	    return ""
	}
    } else {
	${log}::warn "Not listening for HTTP connections on $port!"
    }

    return ""
}


# ::minihttpd::headers -- Return client request headers
#
#       Return the whole set of headers, as requested by a given
#       client.  This procedure will only be able to return data from
#       within callbacks within the time taken for a request to be
#       served.
#
# Arguments:
#	port	Port number of one of our HTTP servers.
#	sock	Socket to client.
#
# Results:
#       Return a list of pairs, ready for an array set command.
#
# Side Effects:
#       None.
proc ::minihttpd::headers { port sock } {
    variable HTTPD
    variable log

    set headers {}
    set idx [lsearch $HTTPD(servers) $port]
    if { $idx >= 0 } {
	set varname "::minihttpd::Server_${port}"
	upvar \#0 $varname Server
	
	set idx [lsearch $Server(clients) $sock]
	if { $idx >= 0 } {
	    set varname "::minihttpd::Client_${port}_${sock}"
	    upvar \#0 $varname Client

	    foreach k [array names Client mime,*]  {
		# Strip away the leading "mime," from the key
		lappend headers [string range $k 5 end] $Client($k)
	    }
	} else {
	    ${log}::warn "$sock is not a recognised client of $port"
	}
    } else {
	${log}::warn "Not listening for HTTP connections on $port!"
    }
    return $headers
}



# ::minihttpd::__form_data -- Extract form data into query
#
#       This procedure extracts the form data that was contained in
#       the body of the POST request to be the arguments of the query.
#       This effectively erase any argument that would have been
#       passed as a query, and does not take into account charset
#       conversions at this stage.
#
# Arguments:
#	port	Port number of one of our HTTP servers.
#	sock	Socket to client.
#
# Results:
#       1 if extraction was made and successful, 0 otherwise
#
# Side Effects:
#       Modifies the client structure.
proc ::minihttpd::__form_data { port sock } {
    variable HTTPD
    variable log

    set idx [lsearch $HTTPD(servers) $port]
    if { $idx >= 0 } {
	set varname "::minihttpd::Server_${port}"
	upvar \#0 $varname Server
	
	set idx [lsearch $Server(clients) $sock]
	if { $idx >= 0 } {
	    set varname "::minihttpd::Client_${port}_${sock}"
	    upvar \#0 $varname Client

	    array set HDR [headers $port $sock]
	    if { [array names HDR content-type] != "" } {
		foreach tken [split $HDR(content-type) ";"] {
		    set tken [string trim $tken]
		    set dta [string trim $Client(data)]
		    if { $tken == "application/x-www-form-urlencoded" \
			     && $dta != "" } {
			set Client(query) [__tokenise_query $dta]
			    
			return 1
		    }
		}
	    }
	} else {
	    ${log}::warn "$sock is not a recognised client of $port"
	}
    } else {
	${log}::warn "Not listening for HTTP connections on $port!"
    }

    return 0
}


proc ::minihttpd::__authorised { port sock url } {
    variable HTTPD
    variable log

    set idx [lsearch $HTTPD(servers) $port]
    if { $idx >= 0 } {
	set varname "::minihttpd::Server_${port}"
	upvar \#0 $varname Server
	
	set idx [lsearch $Server(clients) $sock]
	if { $idx >= 0 } {
	    set varname "::minihttpd::Client_${port}_${sock}"
	    upvar \#0 $varname Client

	    # Run through all the authorisation "zones" that we have
	    # had a registration for to look for one which match the
	    # URL that was requested.  On a match, authorise or reject
	    # incoming client request.
	    foreach {ptn realm auths} $Server(-authorization) {
		if { [string match -nocase $ptn $Client(url)] \
			 && [llength $auths] > 0 } {
		    # We have a match for the URL contained in the
		    # request, either return the realm if we are not
		    # authorising the client, or an empty string if it
		    # should be let through.
		    array set HDR [headers $port $sock]
		    if { [array names HDR authorization] != "" } {
			# Just do Basic authentication at this stage.
			if { [lindex $HDR(authorization) 0] == "Basic" } {
			    set auth [lindex $HDR(authorization) 1]
			    foreach up $auths {
				# This could be optimised...
				if { [::base64::encode $up] == $auth } {
				    return ""
				}
			    }
			}
		    }
		    
		    return $realm
		} 
	    }
	}
    }
    return ""; # Default is to authorise, since we have a catch all
	       # for every URL zone.
}


# ::minihttpd::data -- Return extra request data
#
#       Return the extra data pending after the request, if any.
#
# Arguments:
#	port	Port number of one of our HTTP servers.
#	sock	Socket to client.
#
# Results:
#       None.
#
# Side Effects:
#       None.
proc ::minihttpd::data { port sock } {
    variable HTTPD
    variable log

    set idx [lsearch $HTTPD(servers) $port]
    if { $idx >= 0 } {
	set varname "::minihttpd::Server_${port}"
	upvar \#0 $varname Server
	
	set idx [lsearch $Server(clients) $sock]
	if { $idx >= 0 } {
	    set varname "::minihttpd::Client_${port}_${sock}"
	    upvar \#0 $varname Client
	    
	    return $Client(data)
	} else {
	    ${log}::warn "$sock is not a recognised client of $port"
	}
    } else {
	${log}::warn "Not listening for HTTP connections on $port!"
    }
    return ""
}


proc ::minihttpd::setHeaders { port sock args } {
    variable HTTPD
    variable log

    set idx [lsearch $HTTPD(servers) $port]
    if { $idx >= 0 } {
	set varname "::minihttpd::Server_${port}"
	upvar \#0 $varname Server
	
	set idx [lsearch $Server(clients) $sock]
	if { $idx >= 0 } {
	    set varname "::minihttpd::Client_${port}_${sock}"
	    upvar \#0 $varname Client

	    # Transcript current list of headers into temporary array
	    # (to guarantee we only have one key).
	    array set HDRS {}
	    if { [info exists Client(headers)] } {
		array set HDRS $Client(headers)
	    }
	    # Set each key in the arguments, there is no check
	    # whatsoever.
	    foreach {k v} $args {
		set HDRS($k) $v
	    }
	    # Remember for next time.
	    set Client(headers) [array get HDRS]
	} else {
	    ${log}::warn "$sock is not a recognised client of $port"
	}
    } else {
	${log}::warn "Not listening for HTTP connections on $port!"
    }
    return ""
}



# ::minihttpd::__handler_list -- Pretty prints list of internal handlers
#
#	Describes the list of registered internal handlers within the
#	server in HTML.
#
# Arguments:
#	port	Port number of server
#
# Results:
#	HTML describing handlers in server, empty string on error.
#
# Side Effects:
#	None.
proc ::minihttpd::__handler_list { port } {
    variable HTTPD
    variable log

    set response ""
    set idx [lsearch $HTTPD(servers) $port]
    if { $idx >= 0 } {
	set varname "::minihttpd::Server_${port}"
	upvar \#0 $varname Server

	::html::init
	::html::title "Handlers for server at port $Server(port)"
	append response [::html::head "Server handlers"] "\n"
	append response [::html::bodyTag] "\n"

	append response [::html::openTag table] "\n"
	append response [::html::openTag tr] "\n"
	append response [::html::cell "" "Handlers" th] "\n"
	append response [::html::cell "" "MIME Type" th] "\n"
	append response [::html::closeTag] "\n"

	foreach { ptn cb fmt } $Server(handlers) {
	    append response [::html::openTag tr] "\n"
	    append response [::html::cell "" $ptn] "\n"
	    append response [::html::cell "" $fmt] "\n"
	    append response [::html::closeTag] "\n"
	}
	append response [::html::closeTag] "\n"
	append response [::html::end]
    }

    return $response
}


proc ::minihttpd::__ws_callback { port cb sock type msg } {
    variable HTTPD
    variable log

    # Ugly but working eval...
    if { [catch {eval [concat $cb [list $sock $type $msg]]} res] } {
	${log}::error "Error when executing WebSocket reception\
                       handler: $res"
    }
    if { $type eq "close" } {
	__disconnect $port $sock
    }
}


# ::minihttpd::__push -- Push answer back to client.
#
#	Arrange for answer to be sent back to a client, most of the
#	time, it will be a file and the file is being sent back
#	asynchronously.
#
# Arguments:
#	port	Port number of one of our HTTP servers.
#	sock	Socket to client.
#
# Results:
#	None.
#
# Side Effects:
#	Copy content of local file to requesting socket!
proc ::minihttpd::__push { port sock } {
    variable HTTPD
    variable log

    set idx [lsearch $HTTPD(servers) $port]
    if { $idx >= 0 } {
	set varname "::minihttpd::Server_${port}"
	upvar \#0 $varname Server
	
	set idx [lsearch $Server(clients) $sock]
	if { $idx >= 0 } {
	    set varname "::minihttpd::Client_${port}_${sock}"
	    upvar \#0 $varname Client

	    # Stop listening for incoming data from client at this
	    # stage!
	    fileevent $sock readable ""

	    # Check authorisation of client, a non-empty string will
	    # mean to reject the request and contains the realm to use
	    # for mediating this as part of the transaction.
	    set realm [__authorised $port $sock $Client(url)]
	    if { $realm != "" } {
		__push_error $port $sock 401 "Authorization Required" \
		    [list WWW-Authenticate "Basic realm=\"$realm\""]
		return; # Return ASAP since rejected!
	    }

	    # Convert application/x-www-formencoded into what the
	    # following expects from Client(query).
	    __form_data $port $sock

	    set Client(response) ""
	    set Client(handler) ""
	    set Client(responseType) "text/html"
	    set Client(headers) {}

	    foreach { ptn cb fmt } $Server(handlers) {
		if { [string match -nocase $ptn $Client(url)] } {
		    if { [catch {eval [linsert $cb end $port $sock $Client(url) $Client(query)]} res] } {
			__push_error $port $sock 400 \
			    "Error when executing internal handler: $res"
			return
		    } else {
			set Client(handler) $ptn
			set Client(response) $res
			set Client(responseType) $fmt
		    }
		    break
		}
	    }

	    # Convert the socket to a web socket if appropriate.
	    set Client(live) 0
	    if { $Server(live) ne "" } {
		set Client(live) [::websocket::test $Server(listen) $sock \
				      $Client(url) [headers $port $sock] \
				      $Client(query)]
	    }
	    
	    if { $Client(handler) == "" && !$Client(live) } {
		set mypath ""
		set myurl [fullurl $port $Client(url) mypath]
	    } else {
		set mypath $Client(url)
	    }

	    if { $Server(root) != "" && $Client(handler) == "" \
		     && !$Client(live) && [file isdirectory $mypath] } {
		dirlist::dirlist $port $sock $mypath \
		    [__URLtoString $Client(url)]
	    }

	    if { $Server(root) == "" \
		     && !$Client(live) && $Client(handler) == "" } {
		set Client(response) [__handler_list $port]
	    }

	    if {[string length $mypath] == 0 && $Server(root) != "" } {
		__push_error $port $sock 400 "$Client(url) invalid path"
		return
	    }

	    if { $Client(live)  } {
		::websocket::upgrade $sock
	    } elseif { $Client(response) != "" || $Client(handler) != "" } {
		puts $sock "HTTP/1.0 200 Data follows"
		puts $sock "Date: [__fmtdate [clock seconds]]"
		if { [catch {file mtime $mypath} tm] } {
		    set tm [clock seconds]
		}
		puts $sock "Last-Modified: [__fmtdate $tm]"
		puts $sock "Content-Type: $Client(responseType)"
		puts $sock \
		    "Content-Length: [string length $Client(response)]"
		# Add dynamic headers
		foreach {k v} $Client(headers) {
		    puts $sock "$k: [string trim $v]"
		}
		puts $sock ""
		if { $Client(proto) == "HEAD" } {
		    __finish $port $mypath "" $sock 0
		} else {
		    # Use the event loop to push data to the client.
		    fconfigure $sock \
			-translation binary \
			-blocking $Server(-sockblock)
		    set Client(outbytes) 0
		    fileevent $sock writable \
			[list ::minihttpd::__flush $port $sock]
		}
	    } else {
		if {![catch {open $mypath} in]} {
		    puts $sock "HTTP/1.0 200 Data follows"
		    puts $sock "Date: [__fmtdate [clock seconds]]"
		    puts $sock \
			"Last-Modified: [__fmtdate [file mtime $mypath]]"
		    puts $sock "Content-Type: [::mimetype::guess $mypath]"
		    puts $sock "Content-Length: [file size $mypath]"
		    puts $sock ""
		    if { $Client(proto) == "HEAD" } {
			__finish $port $mypath "" $sock 0
		    } else {
			fconfigure $sock \
			    -translation binary \
			    -blocking $Server(-sockblock)
			fconfigure $in -translation binary -blocking 1
			fcopy $in $sock \
			    -command [list ::minihttpd::__finish \
					  $port $mypath $in $sock]
		    }
		} else {
		    __push_error $port $sock 404 "$Client(url) $in"
		}
	    }
	} else {
	    ${log}::warn "$sock is not a recognised client of $port"
	}
    } else {
	${log}::warn "Not listening for HTTP connections on $port!"
    }
}


# ::minihttpd::__flush -- Flush response to client
#
#       Flush response to client in chunks whenever it is ready to
#       receive more data.  Finalise connection (end!) once done.
#
# Arguments:
#	port	Port number of one of our HTTP servers.
#	sock	Socket to client.
#
# Results:
#       None.
#
# Side Effects:
#       Copy client response in small chunks to the client.
proc ::minihttpd::__flush { port sock } {
    variable HTTPD
    variable log

    set idx [lsearch $HTTPD(servers) $port]
    if { $idx >= 0 } {
	set varname "::minihttpd::Server_${port}"
	upvar \#0 $varname Server
	
	set idx [lsearch $Server(clients) $sock]
	if { $idx >= 0 } {
	    set varname "::minihttpd::Client_${port}_${sock}"
	    upvar \#0 $varname Client

	    set len [string length $Client(response)]
	    if { $Client(outbytes) < $len } {
		puts -nonewline $sock \
		    [string range $Client(response) \
			 $Client(outbytes) \
			 [expr {$Client(outbytes)+$Server(-bufsize)-1}]]
		incr Client(outbytes) $Server(-bufsize)
	    } else {
		__finish $port $Client(url) "" $sock $len
	    }
	}
    }
}


# ::minihttpd::__finish -- Finish serving request
#
#	This command is called back when the content of a file has
#	been copied to the requesting client socket.  The client is
#	disconnected and the transaction is logged.
#
# Arguments:
#	port	Port number of one of our HTTP servers.
#	mypath	Path to file that was copied
#	in	File descriptor to file being copied (empty for HEAD reqs)
#	out	Socket to client.
#	bytes	Number of bytes that were copied.
#	error	Non empty and containing an explanation of errors.
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::minihttpd::__finish { port mypath in out bytes { error {} } } {
    # Close file descriptor of file being sent.
    if { $in != "" } {
	::close $in
    }
    
    # Scream on error, log transaction.
    if { $error != "" } {
	if { [string match "*connection reset*peer*" $error] } {
	    __translog $port $out Error "Connection reset by peer on $mypath"
	} else {
	    __translog $port $out Error "Copying data for $mypath failed"
	}
    } else {
	__translog $port $out Done "$mypath"
    }
    
    # Close connection to client.
    __disconnect $port $out
}


# ::minihttpd::config -- Configure a server
#
#	This command set or get the options of a server.
#
# Arguments:
#	port	Port of server
#	args	list of options
#
# Results:
#	Return all options, the option requested or set the options
#
# Side Effects:
#	None.
proc ::minihttpd::config { port args } {
    variable HTTPD
    variable log

    # Check that this is one of our connections
    set idx [lsearch $HTTPD(servers) $port]
    if { $idx < 0 } {
	${log}::warn "Server $port is not valid"
	return -code error "Identifier invalid"
    }

    set varname "::minihttpd::Server_${port}"
    upvar \#0 $varname Server

    set o [lsort [array names Server "-*"]]

    if { [llength $args] == 0 } {      ;# Return all results
	set result ""
	foreach name $o {
	    lappend result $name $Server($name)
	}
	return $result
    }

    foreach {opt value} $args {        ;# Get onr or set some
	if { [lsearch $o $opt] == -1 } {
	    return -code error "Unknown option $opt, must be: [join $o ", " ]"
	}
	if { [llength $args] == 1 } {  ;# Get one config value
	    return $Server($opt)
	}
	set Server($opt) $value         ;# Set the config value
    }
}


proc ::minihttpd::handler { port path cb { fmt "text/html" } } {
    variable HTTPD
    variable log

    # Check that this is one of our connections
    set idx [lsearch $HTTPD(servers) $port]
    if { $idx < 0 } {
	${log}::warn "Server $port is not valid"
	return -code error "Identifier invalid"
    }

    set varname "::minihttpd::Server_${port}"
    upvar \#0 $varname Server

    lappend Server(handlers) $path $cb $fmt
}


proc ::minihttpd::live { port path cb { proto "*" } } {
    variable HTTPD
    variable log

    # Check that this is one of our connections
    set idx [lsearch $HTTPD(servers) $port]
    if { $idx < 0 } {
	${log}::warn "Server $port is not valid"
	return -code error "Identifier invalid"
    }

    set varname "::minihttpd::Server_${port}"
    upvar \#0 $varname Server

    if { $Server(live) eq "" } {
	set Server(live) [::websocket::server $Server(listen)]
    }
    ::websocket::live $Server(listen) \
	$path \
	[list [namespace current]::__ws_callback $port $cb] \
	$proto
}



# ::minihttpd::defaults -- Set/Get defaults for all new connections
#
#	This command sets or gets the defaults options for all new
#	connections, it will not perpetrate on existing pending
#	connections, use ::minihttpd::config instead.
#
# Arguments:
#	args	List of -key value or just -key to get value
#
# Results:
#	Return all options, the option requested or set the options
#
# Side Effects:
#	None.
proc ::minihttpd::defaults { args } {
    variable HTTPD
    variable log

    set o [lsort [array names HTTPD "-*"]]

    if { [llength $args] == 0 } {      ;# Return all results
	set result ""
	foreach name $o {
	    lappend result $name $HTTPD($name)
	}
	return $result
    }

    foreach {opt value} $args {        ;# Get onr or set some
	if { [lsearch $o $opt] == -1 } {
	    return -code error "Unknown option $opt, must be: [join $o ,]"
	}
	if { [llength $args] == 1 } {  ;# Get one config value
	    return $HTTPD($opt)
	}
	set HTTPD($opt) $value           ;# Set the config value
    }
}


# ::minihttpd::__log -- Handle logging.
#
#	This command will log to the log file associated to the server
#	if there was such.  It will also log through the logger
#	module, at the "info" level.
#
# Arguments:
#	port	Port number of one of our HTTP servers.
#	txt	Text for line.
#
# Results:
#	None.
#
# Side Effects:
#	Will possibly write to the log file.
proc ::minihttpd::__log { port txt } {
    variable HTTPD
    variable log

    set idx [lsearch $HTTPD(servers) $port]
    if { $idx >= 0 } {
	set varname "::minihttpd::Server_${port}"
	upvar \#0 $varname Server

	if { $Server(-logfile) != "" } {
	    if { [catch {open $Server(-logfile) "a+"} fdes] == 0} {
		puts $fdes $txt
		::close $fdes
	    } else {
		${log}::warn "Could not open $Server(-logfile) for logging"
	    }
	}
	${log}::info "HTTP server \#$port: $txt"
    } else {
	${log}::warn "Not listening for HTTP connections on $port!"
    }	
}


# ::minihttpd::__push_error -- Push back error to client.
#
#	This command will send a formatted HTTP error message to the
#	client, explaining the error.
#
# Arguments:
#	port	Port number of one of our HTTP servers.
#	sock	Socket to client
#	code	HTTP error message
#	errmsg	Explanation for the error.
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::minihttpd::__push_error { port sock code { errmsg "" } { hdrs "" } } {
    variable HTTPD
    variable log
    variable HTTPD_errors

    set idx [lsearch $HTTPD(servers) $port]
    if { $idx >= 0 } {
	set varname "::minihttpd::Server_${port}"
	upvar \#0 $varname Server
	
	set idx [lsearch $Server(clients) $sock]
	if { $idx >= 0 } {
	    set varname "::minihttpd::Client_${port}_${sock}"
	    upvar \#0 $varname Client

	    if { $errmsg == "" } {
		set errmsg $HTTPD_errors($code)
	    }
	    append Client(url) ""
	    set message "<title>Error: $code</title>Error <b>$Client(url): $errmsg</b>."
	    puts $sock "HTTP/1.0 $code $errmsg"
	    puts $sock "Date: [__fmtdate [clock seconds]]"
	    puts $sock "Content-Length: [string length $message]"
	    foreach { hdr val } $hdrs {
		puts $sock "$hdr: $val"
	    }
	    puts $sock ""
	    puts $sock $message
	    __translog $port $sock Error $message
	    __disconnect $port $sock;  # Will flush the sock
	} else {
	    ${log}::warn "$sock is not a recognised client of $port"
	}
    } else {
	${log}::warn "Not listening for HTTP connections on $port!"
    }
}

# ::minihttpd::__URLtoString -- Decode url-encoded strings
#
#	This command decodes a url-encoded string and returns its
#	decoded equivalent.
#
# Arguments:
#	data	String to be decode.
#
# Results:
#	Returns the decoded string.
#
# Side Effects:
#	None.
proc ::minihttpd::__URLtoString {data} {
    regsub -all {([][$\\])} $data {\\\1} data
    regsub -all {%([0-9a-fA-F][0-9a-fA-F])} $data  {[format %c 0x\1]} data
    return [subst $data]

    # KEPT BELOW JUST IN CASE, NEVER REACHED...
    # Loops until the result is exactly the same as the input, I
    # *think* that this is a proper behaviour
    for { set src $data } { 1 } { set src $data } {
	regsub -all {([][$\\])} $data {\\\1} data
	regsub -all {%([0-9a-fA-F][0-9a-fA-F])} $data  {[format %c 0x\1]} data
	set data [subst $data]
	if { $data == $src } {
	    return $data
	}
    }
    return $data
}


# ::minihttpd::__StringToURL -- Encode strings for URL communication
#
#	This command encodes a Tcl (Utf8) string into a string that is
#	valid as a URL specification.
#
# Arguments:
#	data	String to be decode.
#
# Results:
#	Returns the encoded string.
#
# Side Effects:
#	None.
proc ::minihttpd::__StringToURL {data} {
    set len [string length $data]
    set res ""
    for { set i 0 } { $i < $len } { incr i } {
	set char [string index $data $i]
	if { [string match "\[0-9a-zA-Z\]" $char] || [string is punc $char] } {
	    append res $char
	} else {
	    append res "[format "%%%02x" [scan $char %c]]"
	}
    }
    return $res
}


# ::minihttpd::__fmtdate -- HTTP data formatting
#
#	This command generates a date string in HTTP format
#
# Arguments:
#	clicks	Time in seconds since a known period of time.
#
# Results:
#	Returns the formatted date.
#
# Side Effects:
#	None.
proc ::minihttpd::__fmtdate {clicks} {
    return [clock format $clicks -gmt 1 -format {%a, %d %b %Y %T %Z}]
}


# ::minihttpd::__translog -- Transaction log.
#
#	This command log an HTTP transaction through the
#	::minihttpd::__log mechanism.
#
# Arguments:
#	port	Port number of one of our servers.
#	sock	Sock to one of its clients.
#	reason	Short name of transaction
#	args	Additional information for transaction.
#
# Results:
#	None.
#
# Side Effects:
#	Will possibly write to the log file.
proc ::minihttpd::__translog { port sock reason args } {
    variable HTTPD

    set logstr "[clock format [clock seconds] -format $HTTPD(dateformat)]"
    # Do not find out about the remote peer since this might imply DNS
    # timeouts as Tcl seems to be doing some reverse DNS to try
    # finding out the name of the IP address.
    if { 0 && [catch {fconfigure $sock -peername} sockinfo] == 0 } {
	append logstr " \[[lindex $sockinfo 1]:[lindex $sockinfo 2]\]"
    }
    append logstr " \[$reason\] "
    append logstr "[join $args { }]"
    __log $port $logstr
}
