##################
## Program Name    --  url_watcher.tcl
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##   This program awaits for connection from remote clients and is
##   able to watch for local file modifications on the host system or
##   remote urls on their behalf.  Depending on the command used for
##   the addition of a watch, the program will be able to wait for the
##   existence of a file/URL or not.
##
##################
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.




# Array Name       --  FW
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Global state of this program
#
# Contents:
#    port	- Port number for watcher service.
#    poll	- Poll period in milliseconds
#    clients 	- List of current socket clients.
#    socket 	- Current server socket
#    err_cb 	- Current error handlers
#    terminate 	- Termination mark
#    paths 	- List of watched paths
#    pathid 	- ID generator
array set FW {
    clients   ""
    socket    ""
    terminate 0
    paths     ""
    pathid    0
    servid    ""
    bursting  ""
}

source [file join [file dirname $argv0] argutil.tcl]
argutil::accesslib tcllib

# Now parse the options and put the result into the global state array
package require cmdline

set options {
    { poll.integer "1000" "Poll period in milliseconds for files" }
    { urlskip.integer "5" "Multiplier of the poll to compute URL poll period" }
    { port.integer "4278" "Port to listen to" }
    { verbose.arg "warn" "Verbosity level" }
}

set inited [argutil::initargs FW $options]
if { [catch {cmdline::typedGetoptions argv $options} optlist] != 0 } {
    puts [cmdline::typedUsage $options "accepts the following options:"]
    exit
}
array set FW $optlist
foreach key $inited {
    argutil::makelist FW($key)
}

# Include modules that we depend on.  This is complicated to be able
# to address separately modules in the verbose specification.
argutil::accesslib til
argutil::loadmodules [list cmdserver] $FW(verbose)

package require http

# Initialise local logging facility
package require logger
set FW(log) [::logger::init url_watcher]
$FW(log)::setlevel $FW(verbose)

argutil::fix_outlog




# Command Name     --  find_path
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Find the identifier of a given path, return an empty string on failure
#
# Arguments:
#    path	- Path to find
proc find_path { path } {
    global FW

    foreach p $FW(paths) {
	set varname "__Path_$p"
	upvar \#0 $varname Path

	if { [string equal $Path(path) $path] } {
	    return $p
	}
    }

    return ""
}


# Command Name     --  path_mtime
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Return the modification time of a path.  This command is intelligent
# and accomodates both for local files and remote URLs (using HTTP
# only).  Return 0 in case the time could not be known.
#
# Arguments:
#    path	- Path which modification time is to be fetched.
proc path_mtime { path { readcheck 0 } } {
    global FW

    set mtime 0
    if { [string match -nocase "http:/*" $path] } {
	if { [catch  "::http::geturl $path -validate 1" token] == 0 } {
	    upvar \#0 $token state
	    if { $state(status) == "ok" } {
		array set meta $state(meta)
		if { [array names meta "Last-Modified"] == "Last-Modified" } {
		    if { [catch "clock scan \"$meta(Last-Modified)\"" \
			      mtime] != 0 } {
			$FW(log)::warn "Could not understand \"$meta(Last-Modified)\" as a date"
		    }
		} else {
		    $FW(log)::warn "URL $path has no date!"
		}
	    } else {
		$FW(log)::warn \
		    "Failed validating $path: [::http::error $token]"
	    }
	    ::http::cleanup $token
	} else {
	    $FW(log)::warn "Cannot initiate URL validation for $path"
	}
    } else {
	if { ( $readcheck && [file readable $path] ) || ! $readcheck } {
	    if { [catch "file mtime $path" mtime] != 0 } {
		$FW(log)::warn "$path not there"
		set mtime 0
	    }
	}
    }

    $FW(log)::debug "Time of $path is $mtime"

    return $mtime
}


# Command Name     --  watch_paths
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Periodically poll all registered paths for modification and deliver
# callbacks as necessary.  This command posts itself using <after>.
proc watch_paths { { paths "" } } {
    global FW

    set modified ""
    set removed ""

    if { $paths == "" } {
	set paths_to_check $FW(paths)
    } else {
	set paths_to_check $paths
    }
    foreach p $paths_to_check {
	set varname "__Path_$p"
	upvar \#0 $varname Path

	if { ! $Path(initpending) } {
	    set mtime [path_mtime $Path(path)]
	    if { [array names Path mtime] != "" } {
		if { $mtime == 0 } {
		    if { $Path(mtime) != 0 } {
			$FW(log)::notice "$Path(path) removed"
			# Send notification to remote clients
			::cmdserver::broadcast $FW(servid) \
			    "REMOVED \"$Path(path)\"" $Path(watch)
			if { $Path(removeonremoval) } {
			    lappend removed $p
			} else {
			    set Path(mtime) 0
			}
		    }
		} elseif { $mtime != $Path(mtime) } {
		    if { $Path(mtime) == 0 } {
			set Path(mtime) $mtime
			$FW(log)::notice "$Path(path) created: $mtime"
			# Send notification to remote clients.
			::cmdserver::broadcast $FW(servid) \
			    "CREATED \"$Path(path)\" $mtime \"[clock format $mtime]\"" \
			    $Path(watch)
		    } else {
			set Path(mtime) $mtime
			$FW(log)::notice "$Path(path) changed: $mtime"
			::cmdserver::broadcast $FW(servid) \
			    "MODIFIED \"$Path(path)\" $mtime \"[clock format $mtime]\"" \
			    $Path(watch)
		    }
		}
	    }
	}
    }

    # Remove paths that should be
    foreach p $removed {
	set idx [lsearch $FW(paths) $p]
	if { $idx >= 0 } {
	    set FW(paths) [lreplace $FW(paths) $idx $idx]
	    set varname "__Path_$p"
	    upvar \#0 $varname Path
	    unset Path
	}
    }

    # Repost next scheduling.
    foreach p $paths {
	set idx [lsearch $removed $p]
	if { $idx < 0 } {
	    set varname "__Path_$p"
	    upvar \#0 $varname Path
	    set Path(afterid) [after $Path(period) "watch_paths $p"]
	}
    }
}


# Command Name     --  check_watch
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# This command check the vision that a client has about a given path against
# our reality.  If the client is late, a modification notification will be
# generated again.  If the path does not exist anymore at our site, we
# generate a remove notification.
#
# Arguments:
#    path	- Path to check
#    mtime	- Known time at remote client.
#    sock	- Socket of client that wants to be notified.
proc check_watch { path mtime sock } {
    global FW

    set p [find_path $path]
    if { $p == "" } {
	::cmdserver::broadcast $FW(servid) "REMOVED $path" $sock
    } else {
	set varname "__Path_$p"
	upvar \#0 $varname Path
	if { $Path(initpending) } {
	    $FW(log)::info "Postponing check for \#$p: $Path(path)"
	    lappend Path(checkrequests) [list $sock $mtime]
	} else {
	    if { $Path(mtime) > $mtime } {
		# Send notification to remote client.
		::cmdserver::broadcast $FW(servid) \
		    "MODIFIED \"$Path(path)\" $Path(mtime) \"[clock format $Path(mtime)]\"" \
		    $sock
	    }
	}
    }
}


# Command Name     --  install_watch
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Finish the installation of a path watch.  This command is designed to be
# lazily called when the process has time in order to make HTTP socket
# retrieval work in the background.  Consequently, it also takes care of
# the "CHECK" requests that might have arrived in the mean time (i.e. between
# the creation of the path in our process and the finalisation of the current
# date, as done by this procedure).
#
# Arguments:
#    p_id	- Identifier of path watch.
proc install_watch { p_id } {
    global FW

    set varname "__Path_$p_id"
    upvar \#0 $varname Path

    # Get the current time of the path.  For URLs, this will generate
    # a HTTP HEAD request.
    set Path(mtime) [path_mtime $Path(path) 1]
    $FW(log)::notice \
	"Modification time of $Path(path) at install is $Path(mtime)"

    if { $Path(mtime) == 0 } {
	# The path does not exist, we deliver the initial "CHECK"
	# requests that could not be serviced and remove the path from
	# the ones that we know.
	$FW(log)::info "Delivering postponed REMOVED sync requests for $p_id"
	foreach req $Path(checkrequests) {
	    set sock [lindex $req 0]
	    ::cmdserver::broadcast $FW(servid) "REMOVED $Path(path)" $sock
	}
	set Path(checkrequests) ""

	if { $Path(removeonremoval) } { 
	    # Remove paths from our list of known path.
	    set idx [lsearch $FW(paths) $p_id]
	    if { $idx >= 0 } {
		set FW(paths) [lreplace $FW(paths) $idx $idx]
	    }
	    unset Path
	} else {
	    # Schedule path watching.
	    set Path(afterid) [after $Path(period) "watch_paths $p_id"]
	}
    } else {
	# Service the CHECK requests that could not be serviced since we
	# were initialising and did not know anything about the modification
	# date yet.
	foreach req $Path(checkrequests) {
	    set sock [lindex $req 0]
	    $FW(log)::info \
		"Delivering postponed MODIFIED sync requests for $p_id: $Path(mtime) > [lindex $req 1]"
	    if { $Path(mtime) > [lindex $req 1] } {
		::cmdserver::broadcast $FW(servid) \
		    "MODIFIED $Path(path) $Path(mtime) \"[clock format $Path(mtime)]\"" \
		    $sock
	    }
	}
	set Path(checkrequests) ""

	# Schedule path watching.
	set Path(afterid) [after $Path(period) "watch_paths $p_id"]
    }

    # Now, remember that initialisation has been performed
    set Path(initpending) 0
}


# Command Name     --  add_watch
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Add a path watcher to the list of current watchers.  This routine
# handles correctly several clients for a single path.  Return 1
# on success, 0 in case of error.
#
# Arguments:
#    path	- Path to be watched.
#    sock	- Socket of client that wants to be notified.
#    period	- Checking period, empty means default.
#    onlyifexistent	- Add the watch point only if the file exists.
proc add_watch { path sock period onlyifexistent forcelater} {
    global FW

    set mtime 0
    set appended 0

    # Check within our known list of path whether we already have a
    # registration for this path.
    foreach p $FW(paths) {
	set varname "__Path_$p"
	upvar \#0 $varname Path
	
	if { $Path(path) == $path } {
	    set appended 1
	    # If the path already existed, simply add the client
	    # socket to the list of watchers if it did not already exist.
	    set idx [lsearch $Path(watch) $sock]
	    if { $idx < 0 } {
		lappend Path(watch) $sock
	    }
	}
    }


    # Otherwise remember information for this path and schedule
    # initialisation as soon as possible.
    if { ! $appended } {
	set p [incr FW(pathid)]

	set varname "__Path_$p"
	upvar \#0 $varname Path

	if { [string match -nocase "http:/*" $path] } {
	    set Path(path) $path
	    set Path(period) $FW(poll)
	} else {
	    set Path(path) [::disk_util::absolute_path $path]
	    set Path(period) [expr $FW(urlskip) * $FW(poll)]
	}
	if { $period != "" } {
	    set Path(period) $period
	}
	set Path(initpending) 1
	set Path(checkrequests) ""
	set Path(watch) $sock
	set Path(mtime) 0
	set Path(removeonremoval) $onlyifexistent
	set idx [lsearch $FW(bursting) $sock]
	if { $forcelater || $idx >= 0 } {
	    set when [expr int (rand() * $Path(period))]
	    $FW(log)::notice "Installing watch for $path in $when ms"
	    set Path(afterid) [after $when "install_watch $p"]
	} else {
	    $FW(log)::notice "Installing watch for $path now"
	    set Path(afterid) [after idle "install_watch $p"]
	}
	lappend FW(paths) $p
    }

    return 1
}


# Command Name     --  remove_watch
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Add a path watcher to the list of current watchers.  This routine
# handles correctly several clients for a single path.  Return the
# 0 if not removed, 1 if removed client, 2 if removed path.
#
# Arguments:
#    path	- Path to be removed
#    sock	- Socket of client that wants to be removed.
proc remove_watch { path sock } {
    global FW

    set removed 0
    set newpaths ""
    foreach p $FW(paths) {
	set varname "__Path_$p"
	upvar \#0 $varname Path
	
	if { $Path(path) == $path } {
	    set idx [lsearch $Path(watch) $sock]
	    if { $idx >= 0 } {
		set Path(watch) [lreplace $Path(watch) $idx $idx]
		set removed 1
	    }
	    if { [llength $Path(watch)] == 0 } {
		# Unpost any possible scheduled procedure call that
		# has something to do with this very path.  This
		# includes both the regular check and the
		# initialisation of path information.
		if { [array names Path "afterid"] == "afterid" } {
		    after cancel $Path(afterid)
		}
		# And remove all what we know about the path.
		unset Path
		set removed 2
	    }
	}

	if { $removed < 2 } {
	    lappend newpaths $p
	}
    }

    set FW(paths) $newpaths

    return $removed
}


# Command Name     --  incoming_cmd
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Command called back for each new command coming from a client.
#
# Arguments:
#    sock	- Socket connection with client.
proc incoming_cmd { sock cmd remaining } {
    global FW

    set idx [lsearch $FW(bursting) $sock]
    switch $cmd {
	WATCH {
	    set file [lindex $remaining 0]
	    set period [lindex $remaining 1]
	    if { $idx < 0 || 1 } {
		add_watch $file $sock $period 0 0
	    } else {
		after idle "add_watch $file $sock \"$period\" 0 1"
	    }
	}
	ADD {
	    set file [lindex $remaining 0]
	    set period [lindex $remaining 1]
	    if { $idx < 0 || 1 } {
		add_watch $file $sock $period 1 0
	    } else {
		after idle "add_watch $file $sock \"$period\" 1 1"
	    }
	}
	REMOVE {
	    foreach file $remaining {
		if { $idx < 0 || 1 } {
		    remove_watch $file $sock
		} else {
		    after idle "remove_watch $file $sock"
		}
	    }
	}
	CHECK {
	    foreach finfo $remaining {
		set path [lindex $finfo 0]
		set mtime [lindex $finfo 1]
		if { $idx < 0 || 1 } {
		    check_watch $path $mtime $sock
		} else {
		    after idle "check_watch $path $mtime $sock"
		}
	    }
	}
	BURST {
	    if { $idx < 0 } {
		lappend FW(bursting) $sock
	    }
	}
	ATONCE {
	    if { $idx >= 0 } {
		set FW(bursting) [lreplace $FW(bursting) $idx $idx]
	    }
	}
    }
}



# Command Name     --  remove_client
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Command called back when a client has closed connection with this server.
#
# Arguments:
#    sock	- Socket of client to be removed.
proc remove_client { servid sock ip port } {
    global FW
    
    # Make a copy of the paths list and remove all
    # notifications for this client.  We need to copy since
    # the list can be modified when removing the watches.
    set pcopy $FW(paths)
    foreach p $pcopy {
	set varname "__Path_$p"
	upvar \#0 $varname Path
	
	remove_watch $Path(path) $sock
    }
}


set FW(servid) [::cmdserver::new $FW(port) \
		    [list WATCH ADD REMOVE CHECK BURST ATONCE] \
		    incoming_cmd "URL/file watcher"]
::cmdserver::connections_cb $FW(servid) "" remove_client

vwait FW(terminate)
