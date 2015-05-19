##################
## Program Name    --  logviewer.tcl
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##    Duplicate the log files from a remote starter into some local files.
##
##################
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

array set LV {
    finish         0
    permid         ""
    remotes        ""
    svcs           ""
    menuorder      "debug info notice warn error critical"
    nbook          ""
    trigger        ""
    liveness       0
}

array set ColourMatch {
    debug     "blue"
    info      "sea green"
    notice    "deep pink"
    warn      "dark orange"
    error     "red"
    critical  "maroon"
}

source [file join [file dirname $argv0] .. bin argutil.tcl]
argutil::accesslib tcllib

# Now parse the options and put the result into the GE global state array
package require cmdline

set options {
    { verbose.arg "warn" "Verbosity Level" }
    { host.arg "localhost" "Host name of server to connect to" }
    { port.integer "3448" "Port number of server to connect to" }
    { services.arg "*" "List of patterns that remotes services should match to be monitored" }
    { period.integer "60" "Period in seconds for process list info trigger" }
}

set inited [argutil::initargs LV $options]
if { [catch {cmdline::typedGetoptions argv $options} optlist] != 0 } {
    puts [cmdline::typedUsage $options "accepts the following options:"]
    exit
}
array set LV $optlist
foreach key $inited {
    argutil::makelist LV($key)
}

# Include modules that we depend on.  This is complicated to be able
# to address separately modules in the verbose specification.
argutil::accesslib til
argutil::loadmodules [list permclient diskutil outlog] $LV(verbose)
package require BWidget


# Initialise local logging facility
package require logger
set LV(log) [::logger::init logview]
$LV(log)::setlevel $LV(verbose)
argutil::fix_outlog


# ::net:list -- Trigger remote list
#
#	This procedure trigger the generation of the remote list of
#	processes under control.  The trigger can be established on a
#	regular basis.
#
# Arguments:
#	id	Identifier of the permanent connection
#	period	Period for trigger, negative to do it once only.
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc net:list { id { period -1 } } {
    global LV

    $LV(log)::info "Triggering remote list"
    set LV(old_remotes) $LV(remotes)
    set LV(remotes) ""
    ::permclient::write $id "LIST"

    if { $period >= 0 } {
	incr LV(liveness) -1
	if { $LV(liveness) <= 0 } {
	    ${log}::warn "Connection to server probably lost"
	    wm title . "LOG Viewer: $LV(host):$LV(port) (Connection Lost)"
	}
	set LV(trigger) [after $period net:list $id $period]
    }
}


# ::net:open -- Connection Opening Trigger
#
#	This command is called each time the connection to the client
#	is opened. We trigger list information to know about the
#	current remote programs.
#
# Arguments:
#	id	Identifier of the permanent connection
#	sock	Socket to client
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc net:open { id sock } {
    global LV

    # Trigger a new request for the list of services.  Just do it once
    # since we already have a regular polling set up.
    net:list $id
    wm title . "LOG Viewer: $LV(host):$LV(port)"
}


# ::ui:popup -- Pop Up menu
#
#	This command pops up the popup menu that is associated to the
#	panes or to the main window.
#
# Arguments:
#	X	X position at which to popup the menu
#	Y	Y position at which to popup the menu
#	pane	Pane to popup the menu for, empty for main log window.
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ui:popup { X Y { pane "" } } {
    global LV ColourMatch

    if { $pane eq "" } {
	tk_popup .popup $X $Y
    } else {
	if { [$LV(nbook) raise] eq $pane } {
	    tk_popup .popup${pane} $X $Y
	}
    }
}


# ::ui:outlog -- Construct UI for log dump
#
#	This procedure contructs the UI components that will be needed
#	for dumping the log from a given known service.
#
# Arguments:
#	sid	Identifier of the service
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ui:outlog { sid } {
    global LV ColourMatch

    set varname "__Service_$sid"
    upvar \#0 $varname Service

    # Build the main notebook if it does not exist.
    if { $LV(nbook) eq "" } {
	set pn [$LV(pane) add -weight 1]
	NoteBook ${pn}.n -height 300
	set LV(nbook) ${pn}.n
	set pack 1
    } else {
	set pack 0
    }
    
    # Add a new tab to the notebook
    $LV(nbook) insert end $sid -text $Service(name)
    $LV(log)::debug "Created new page $sid"

    # Create a text widget and a scrollbar within that tab.
    set pane [$LV(nbook) getframe $sid]
    scrollbar $pane.yview -command "$pane.t yview"
    text $pane.t -yscrollcommand "$pane.yview set"
    bind $pane.t <MouseWheel> \
	{%W yview scroll [expr {int(pow(%D/-120,3))}] units}
    set Service(scrolling) 0
    bind $pane.yview <ButtonPress> \
	"+set __Service_${sid}(scrolling) 1"
    bind $pane.yview <ButtonRelease> \
	"+set __Service_${sid}(scrolling) 0"
    pack $pane.t -side left -fill both -expand on
    pack $pane.yview -side right -fill y
    # Make sure to declare the colours of the log levels.
    foreach type [array names ColourMatch] {
	$pane.t tag configure $type \
	    -foreground $ColourMatch($type)
    }
    
    # Pack the main notebook if necessary
    if { $pack } {
	pack $LV(pane) -fill both -expand on
	pack $LV(nbook) -fill both -expand on
	$LV(nbook) raise $sid
	$LV(nbook) bindtabs <Button-3> [list ui:popup %X %Y]
    }
    
    # Create a popup menu that will control the variables that control
    # what is to be output to the text widget.
    set mn [menu .popup${sid} -tearoff 0 \
		-activeborderwidth 0]
    if { 0 } {
	foreach type $LV(menuorder) {
	    set Service(out_${type}) 1
	    $mn add checkbutton -label "$type" \
		-variable __Service_${sid}(out_${type}) -state disabled
	}
    }
}


# ::net:svccommand -- Send service command
#
#	Send a service directed command to the server to which we are
#	connected.  Service directed commands can only be sent for
#	services that exist, and they take the identifier of the
#	service as an identifier.
#
# Arguments:
#	sid	Identifier of service
#	cmd	Command to send (service id will be appended automatically)
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc net:svccommand { sid cmd } {
    global LV

    set idx [lsearch $LV(svcs) $sid]
    if { $idx >= 0 } {
	set varname "__Service_$sid"
	upvar \#0 $varname Service

	::permclient::write $LV(permid) "$cmd $sid"
	net:list $LV(permid)
    }
}


# ::net:command -- Send command
#
#	Send a command to the server to which we are currently
#	connected, and trigger a list of services again.
#
# Arguments:
#	cmd	Command to send to server
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc net:command { cmd } {
    global LV

    ::permclient::write $LV(permid) $cmd
    net:list $LV(permid)
}


# ::svc:remove -- Remove service
#
#	Remove a service and all its associated UI components.
#
# Arguments:
#	sid	Identifier of the service
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc svc:remove { sid } {
    global LV

    set idx [lsearch $LV(remotes) $sid]
    if { $idx < 0 } {
	set idx [lsearch $LV(svcs) $sid]
	if { $idx >= 0 } {
	    set varname "__Service_$sid"
	    upvar \#0 $varname Service
	    # Remove menu entry for service
	    set idx [.mnu.services index "$sid - $Service(name)"]
	    if { $idx ne "none" } {
		set m [.mnu.services entrycget $idx -menu]
		.mnu.services delete $idx
		destroy $m
	    }
	    # Remove tab for service
	    $LV(nbook) delete $sid
	    # Remove context for service and suppress it from our list
	    # of known services.
	    unset Service
	    set LV(svcs) [lreplace $LV(svcs) $idx $idx]
	}
    }
}


# ::net:connect -- Connect to (new) server
#
#	This command connects to a (new) service controller server,
#	establishing the necessary bindings for service discovery and
#	display.
#
# Arguments:
#	host	Host name or IP of server
#	port	Port number at server
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc net:connect { host { port 3448 }} {
    global LV

    # Remove all known services if any
    foreach sid $LV(svcs) {
	svc:remove $sid
    }

    # Destroy current permanent connection, if any
    if { $LV(permid) ne "" } {
	::permclient::delete $LV(permid)
    }

    # Establish new permanent connection to server
    set LV(host) $host
    set LV(port) $port
    set LV(permid) [::permclient::new $host $port net:incoming \
			-open net:open -poll 5]

    # Trigger periodic check of the list of remote services, if requested
    if { $LV(trigger) ne "" } {
	after cancel $LV(trigger)
	set LV(trigger) ""
    }

    if { $LV(period) > 0 } {
	set period [expr $LV(period) * 1000]
	set LV(trigger) [after $period net:list $LV(permid) $period]
    }

}


# ::ui:connect -- Build window for new connection
#
#	This procedure creates a window for the entry of a new host
#	name and a port numer.  When OK is pressed, the application
#	disconnects from the current server and connects to a new
#	server.
#
# Arguments:
#	wname	Name of top window to create
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ui:connect { {wname .connect} } {
    global LV

    toplevel ${wname}
    wm title ${wname} "Connection to Remote"
    label ${wname}.top \
	-text "Enter the details for another remote service controller"
    frame ${wname}.middle
    frame ${wname}.bottom
    pack ${wname}.top ${wname}.middle ${wname}.bottom -side top

    label ${wname}.middle.adrl -text "Host"
    entry ${wname}.middle.adre
    label ${wname}.middle.prtl -text "Port"
    entry ${wname}.middle.prte
    ${wname}.middle.prte insert 0 $LV(port)
    foreach w [list adrl adre prtl prte] {
	pack ${wname}.middle.$w -side left
    }

    button ${wname}.bottom.ok -text "Ok" \
	-command "net:connect \[${wname}.middle.adre get\] \[${wname}.middle.prte get\]; destroy $wname"
    button ${wname}.bottom.cancel -text "Cancel" -command "destroy ${wname}"
    foreach w [list ok cancel] {
	pack ${wname}.bottom.$w -side left -padx 2 -pady 3 -ipadx 4
    }
}


# ::net:incoming -- Treat incoming commands
#
#	Command called back for each line coming from the remote
#	starter process.  The command from the server is the first
#	word of the line.  When LIST arrives, we check the list of
#	currently knonw services here against those at the server and
#	we remove services that have disappeared.  When INFO arrives,
#	we update our local knowledge about the services and build the
#	UI for dumping the log lines.  When LOG arrives, we dump the
#	log lines into the appropriate windows, according to the
#	choices made by the user when it comes to log levels.
#
# Arguments:
#	id	Identifier of the permanent connection
#	line	Incoming line.
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc net:incoming { id line } {
    global LV ColourMatch

    $LV(log)::debug "Received: $line"
    set spc [string first " " $line]
    if { $spc >= 0 } {
	# Isolate the command.
	set cmd [string toupper [string range $line 0 [expr $spc - 1]]]
	
	switch $cmd {
	    LIST {
		# The list of remote services arrives. Remove services
		# that were registered and have disappeared and
		# request information for each (new) one.
		set LV(remotes) ""
		set l [string range $line [expr $spc + 1] end]
		for { set i 0 } { $i < [llength $l] } { incr i 2 } {
		    lappend LV(remotes) [lindex $l $i]
		}
		# De-register old services.
		foreach sid $LV(old_remotes) {
		    svc:remove $sid
		}
		::permclient::write $LV(permid) "INFO $LV(remotes)"
		set LV(liveness) 3
	    }
	    INFO {
		# One remote service information arrives.  Register
		# this service if it matches our list of authorised
		# local services and open a local log file for
		# writing.
		set sid [lindex $line 1]
		set pids [lindex $line 2]
		set remote_log [lindex $line 3]
		set prg [lrange $line 4 end]
		
		# Check whether the name of the remote program matches
		# our list of authorised patterns, only those that
		# match will be registered as local services.
		foreach ptn $LV(services) {
		    if { [string match $ptn $prg] } {
			# Create/access the global instance of that
			# service.
			set varname "__Service_$sid"
			upvar \#0 $varname Service

			# Initialise things if this is a new service.
			set idx [lsearch $LV(svcs) $sid]
			if { $idx < 0 } {
			    set Service(name) \
				[file rootname [file tail $remote_log]]
			    ui:outlog $sid
			    lappend LV(svcs) $sid
			}

			# (re)initialise with the values from the
			# remote service. The name of the log file
			# will be the same.
			set Service(sid) $sid
			set Service(pids) $pids
			set Service(remote_log) $remote_log

			# Make sure that we start watching the remote
			# service.  starter.tcl gracefully accepts
			# several WATCH commands for the same id.
			::permclient::write $LV(permid) "WATCH $sid"
			
			if { ! [winfo exist .mnu.services.$sid] } {
			    set m [menu .mnu.services.$sid -tearoff 0]
			    .mnu.services add cascade \
				-label "$sid - $Service(name)" \
				-menu .mnu.services.$sid
			    $m add command -label "Remove" -command \
				[list net:svccommand $sid REMOVE]
			    $m add command -label "Restart" -command \
				[list net:svccommand $sid RESTART]
			}
		    }
		}
	    }
	    LOG {
		# Incoming log command.  If it is one of our
		# recognised services, forward the line to the local
		# log.
		set snd_spc [string first " " $line [expr $spc + 1]]
		if { $snd_spc >= 0 } {
		    set sid [string trim [string range $line $spc $snd_spc]]
		    set idx [lsearch $LV(svcs) $sid]
		    if { $idx >= 0 } {
			# This log line comes from one of our
			# registered and authorised services, forward
			# to the local log file.
			set varname "__Service_$sid"
			upvar \#0 $varname Service
			set pane [$LV(nbook) getframe $sid]
			set txt [string range $line [expr $snd_spc + 1] end]
			set tag ""
			foreach type [array names ColourMatch] {
			    set t_idx [string first "\[${type}\]" $txt]
			    if { $t_idx >= 0 } {
				set tag $type
				set close [string last "\]" $txt $t_idx]
				set open [string last "\[" $txt $close]
				set module [string range $txt \
						[expr $open + 1] \
						[expr $close - 1]]
			    }
			}
                        set inserted_sub 0
                        set inserted_main 0
			if { $tag ne "" } {
			    set mn [regsub -all {\s} $module ""]

			    # Look in main popup if we already have a
			    # cascade menu for that module.
			    set len [.popup${sid} index end]
			    set sub -1
			    if { $len ne "none" } {
				for { set i 0 } { $i <= $len } { incr i } {
				    set elabel \
					[.popup${sid} entrycget $i -label]
				    if { $elabel eq $module } {
					set sub $i
					break
				    }
				}
			    }

			    # If not, create a cascade sub menu that
			    # will allow us to fine-grain control the
			    # output in the widget.
			    if { $sub < 0 } {
				menu .popup${sid}.${mn} -tearoff 0 \
				    -activeborderwidth 0
				foreach type $LV(menuorder) {
				    set Service(out_${mn}_${type}) 1
				    .popup${sid}.${mn} add checkbutton \
					-label "$type" \
					-variable __Service_${sid}(out_${mn}_${type}) \
					-state disabled
				}
				.popup${sid} add cascade \
				    -menu .popup${sid}.${mn} \
				    -label $module
			    }

			    # Now, change the meny entry of the sub
			    # cascaded menu to be "normal" as soon as
			    # we get to know something about that log
			    # level.
			    set idx [lsearch $LV(menuorder) $tag]
			    if { [.popup${sid}.${mn} entrycget $idx -state] \
				     ne "normal" } {
				.popup${sid}.${mn} entryconfigure $idx \
				    -state normal
			    }
			    if { [.popup entrycget $idx -state] ne "normal" } {
				.popup entryconfigure $idx -state normal
			    }
			    
			    # And insert the text into the widget only
			    # if the GUI control variable tells us so.
			    if { $Service(out_${mn}_${tag}) } {
				$pane.t insert end "$txt\n" $tag
                                set inserted_sub 1
			    }
			    if { $LV(out_${tag}) } {
				$LV(textwidget) insert end \
				    "$Service(name): $txt\n" $tag
                                set inserted_main 1
			    }
			} else {
			    $pane.t insert end "$txt\n"
                            set inserted_sub 1
			    $LV(textwidget) insert end \
				"$Service(name): $txt\n"
			}
			if { ! $Service(scrolling) && $inserted_sub } {
			    $pane.t yview end
			}
			if { ! $LV(scrolling) && $inserted_main } {
			    $LV(textwidget) yview end
			}
		    }
		}
	    }
	}
    }
}


# ::ui:create -- Create main UI window
#
#	This procedure will create the main UI window and fill in with
#	the appropriate controls.
#
# Arguments:
#	None.
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ui:create {} {
    global LV

    wm title . "LOG Viewer: $LV(host):$LV(port)"

    set LV(pane) [PanedWindow .pw -side left]
    set top [$LV(pane) add -minsize 100 -weight 1]

    scrollbar ${top}.yview -command "${top}.t view"
    set LV(textwidget) [text ${top}.t -yscrollcommand "${top}.yview set" \
			    -height 8]
    bind ${top}.t <MouseWheel> \
	{%W yview scroll [expr {int(pow(%D/-120,3))}] units}
    set LV(scrolling) 0
    bind ${top}.yview <ButtonPress> "+set LV(scrolling) 1"
    bind ${top}.yview <ButtonRelease> "+set LV(scrolling) 0"
    pack ${top}.t -side left -fill both -expand on
    pack ${top}.yview -side right -fill y
    foreach type [array names ColourMatch] {
	${top}.t tag configure $type \
	    -foreground $ColourMatch($type)
    }

    menu .popup -tearoff 0 -activeborderwidth 0
    foreach type $LV(menuorder) {
	set LV(out_${type}) 1
	.popup add checkbutton -label "$type" -variable LV(out_${type}) \
	    -state disabled
    }
    bind $LV(textwidget) <Button-3> [list ui:popup %x %Y]

    menu .mnu
    . configure -menu .mnu

    # File
    set m [menu .mnu.file -tearoff 0]
    .mnu add cascade -label "File" -menu $m
    $m add command -label "Connect" -command ui:connect
    $m add separator
    $m add command -label "Exit" -command exit

    # Services
    set m [menu .mnu.services -tearoff 0]
    .mnu add cascade -label "Services" -menu $m

    # Control
    set m [menu .mnu.control -tearoff 0]
    .mnu add cascade -label "Control" -menu $m
    $m add command -label "Reload" -command "net:command RELOAD"
    $m add command -label "Restart All" -command "net:command RESTART_ALL"
    $m add command -label "Remove All" -command "net:command REMOVE_ALL"
}


ui:create
net:connect $LV(host) $LV(port)
