##################
## Module Name     --  file_generator.tcl
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##    This program generates one or more files in a directory.  The
##    files will grow at an approximate given rate for a period of time
##    (could be infinity) and will then stop growing.
##
##################
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.


# Array Name       --  FG
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Global state of this program.
#
# Contents:
#    arg1	-
#    arg2	-
array set FG {
    finished    0
    fname       ""
    fd          ""
    counter     0
    lastgrowth  0
}

source ../bin/argutil.tcl
argutil::accesslib tcllib 1.6.1

# Now parse the options and put the result into the global state array
package require cmdline

set options {
    { verbose.integer "2" "Verbosity Level" }
    { growth.integer "10240" "Growth rate in bytes per second" }
    { period.integer "100" "Period to decide when to grow (in ms)" }
    { die.integer "60" "Time (in seconds) after which we should die or generate the next file, negative means infinity" }
    { counter.integer "0" "Starting value for file counter" }
    { path.arg "rndcontent%d.bin" "Path to the generated file, %d will be replaced by a counter and generate several files" }
}

if { [catch {cmdline::typedGetoptions argv $options} optlist] != 0 } {
    puts [cmdline::typedUsage $options "accepts the following options:"]
    exit
}
array set FG $optlist

# Include modules that we depend on.  This is complicated to be able
# to address separately modules in the verbose specification.
argutil::accesslib til
argutil::loadmodules [list diskutil] $FG(verbose)



# Command Name     --  push_data
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Push new random binary content in current file.  Open file if it
# does not exist
proc push_data { } {
    global FG

    # Generate new file name and open the next file if possible.
    if { $FG(fd) == "" } {
	set FG(fname) [format $FG(path) $FG(counter)]
	incr FG(counter)

	set res [catch "open $FG(fname) w" FG(fd)]
	if { $res != 0 } {
	    set FG(fd) ""
	    puts "Could not open $FG(fname) for writing, exiting"
	    exit
	} else {
	    puts "Starting to fill in $FG(fname)"
	}
	fconfigure $FG(fd) -translation binary -encoding binary
    }
    
    # Compute how many bytes we should push into the file
    set now [clock clicks -milliseconds]
    set since_last [expr $now - $FG(lastgrowth)]
    set nb_bytes [expr int ($since_last * ( $FG(growth) / 1000.0 ))]

    # Push random binary bytes into the file.
    for { set i 0 } { $i < $nb_bytes } { incr i } {
	puts -nonewline $FG(fd) [binary format c [expr int(256*rand())]]
    }
    flush $FG(fd)

    # Remember the time for next time we grow.
    set FG(lastgrowth) $now

    # Schedule ourselves again
    after $FG(period) push_data
}


# Command Name     --  next_file
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Close current file, if any and go on with the next one or exit if we
# would overwrite the previous one.
proc next_file { } {
    global FG

    # Close current file if any.
    if { $FG(fd) != "" } {
	puts "Closing $FG(fname)"
	close $FG(fd)
	set FG(fd) ""
    }
    
    # See if the next name will be the same as the current one, if so,
    # exit, we are done. That meant that the command line did not
    # require for several files to be generated.
    set nextname [format $FG(path) $FG(counter)]
    if { $nextname == $FG(fname) } {
	puts "done"
	exit
    }
    
    # Otherwise, remember what time it is and start over again.
    set FG(lastgrowth) [clock clicks -milliseconds]
    after [expr $FG(die) * 1000] next_file
}


set FG(lastgrowth) [clock clicks -milliseconds]
after $FG(period) push_data
if { $FG(die) > 0 } {
    after [expr $FG(die) * 1000] next_file
}

vwait FG(finished)

