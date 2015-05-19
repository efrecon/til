##################
## Module Name     --  audio_player
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
## Commands Exported:
##	cmd1
##	cmd2
##################
# Copyright (c) 2004-2005 by the Swedish Institute of Computer Science.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.



# Array Name       --  AUDIO
# Original Author  --  Emmanuel Frecon - emmanuel@sics.se
#
# Global state of this program.
#
# Contents:
#    arg1	-
#    arg2	-
array set AUDIO {
    dns         "ns.sics.se"
    servid      ""
    finished    0
    snd         ""
    urlcache    ""
    tmpfname    ""
}

source [file join [file dirname $argv0] argutil.tcl]
argutil::accesslib tcllib

# Now parse the options and put the result into the GE global state array
package require cmdline

set options {
    { allow.arg "*" "List of hostname/ip allow patterns for incoming clients" }
    { deny.arg "" "List of hostname/ip deny patterns for incoming clients" }
    { verbose.arg "notice" "Verbosity Level" }
    { port.integer "2622" "Port number of server to connect to" }
    { rate.integer "16000" "Default playing rate for audio files" }
    { cache.arg "%progdir%/caches/%progname%" "Directory for local cache, empty for disabling" }
    { cachesize.integer "256" "Cache size, in MB" }
}

set inited [argutil::initargs AUDIO $options]
if { [catch {cmdline::typedGetoptions argv $options} optlist] != 0 } {
    puts [cmdline::typedUsage $options "accepts the following options:"]
    exit
}
array set AUDIO $optlist
foreach key $inited {
    argutil::makelist AUDIO($key)
}

# Include modules that we depend on.  This is complicated to be able
# to address separately modules in the verbose specification.
argutil::accesslib til
argutil::accesslib tcludp
argutil::loadmodules [list cmdserver dnsresolv urlcache \
			 massgeturl diskutil] $AUDIO(verbose)

# Initialise local logging facility
package require logger
set AUDIO(log) [::logger::init audioplayer]
$AUDIO(log)::setlevel $AUDIO(verbose)

argutil::fix_outlog

package require snack

# Guess the DNS server that can be used for address resolution on this
# very machine.  The code below should work on all platforms, since
# they all support nslookup (except MacOS <= 9).  If we do not manage,
# default to something here at SICS!
$AUDIO(log)::notice "Guessing DNS server address..."
set dns ""
set res [catch "::dnsresolv::init $AUDIO(dns)" dns]
if { $res == 0 && $dns != "" } {
    $AUDIO(log)::notice "Found as $dns"
} else {
    $AUDIO(log)::warn "DNS server not found, will have a hard time running!..."
}

proc sound_stop { } {
    global AUDIO

    if { $AUDIO(snd) != "" } {
	$AUDIO(snd) stop
	$AUDIO(snd) destroy
	set AUDIO(snd) ""
    }

    if { $AUDIO(tmpfname) != "" } {
	file delete $AUDIO(tmpfname)
	set AUDIO(tmpfname) ""
    }
}


proc sound_play { url fname sock } {
    global AUDIO
    
    if { $url != $AUDIO(pending) } {
	::cmdserver::broadcast $AUDIO(servid) \
	    "ERROR $url \"Too long waiting time\"" $sock
	return ""
    }

    if { [catch {::snack::sound -rate $AUDIO(rate) \
		     -file $fname} snd] } {
	::cmdserver::broadcast $AUDIO(servid) \
	    "ERROR $url \"$snd\"" $sock
	set AUDIO(snd) ""
    } else {
	if { [catch {$snd play} err] } {
	    ::cmdserver::broadcast $AUDIO(servid) \
		"ERROR $url \"$err\"" $sock
	    set AUDIO(snd) ""
	} else {
	    ::cmdserver::broadcast $AUDIO(servid) \
		"PLAYING $url" $sock
	    set AUDIO(snd) $snd
	}
    }

    return $AUDIO(snd)
}

proc direct_play { sock cid url status token { txt "" } } {
    sound_play $url $AUDIO(tmpfname) $sock
}


proc cache_play { pending sock cid url status { fname "" } } {
    if { $status == "OK" } {
	sound_play $pending $fname $sock
    }
}


proc incoming { sock cmd remaining } {
    global AUDIO

    switch $cmd {
	PLAY {
	    sound_stop

	    set AUDIO(pending) $remaining
	    if { $AUDIO(urlcache) == "" } {
		if { [string match -nocase "http://*" $AUDIO(pending)] } {
		    set AUDIO(tmpfname) \
			[::diskutil::temporary_file "audioplay" "tmp"]
		    ::cmdserver::broadcast $AUDIO(servid) \
			"DOWNLOADING $AUDIO(pending)" $sock
		    ::massgeturl::infile $AUDIO(pending) $AUDIO(tmpfname) \
			[list direct_play $sock] -retries 0
		} else {
		    sound_play $AUDIO(pending) $AUDIO(pending) $sock
		}
	    } else {
		# XXX: We should use the flow mechanism, but this will
		# be most often right.
		set info [::urlcache::urlinfo $AUDIO(urlcache) $AUDIO(pending)]
		if { $info == "" } {
		    ::cmdserver::broadcast $AUDIO(servid) \
			"DOWNLOADING $AUDIO(pending)" $sock
		}
		::urlcache::open $AUDIO(urlcache) $AUDIO(pending) \
		    [list cache_play $AUDIO(pending) $sock]
	    }
	}
	STOP {
	    sound_stop
	}
    }
}

# Create URL cache
if { $AUDIO(cache) != "" } {
    set AUDIO(urlcache) \
	[::urlcache::new [::diskutil::fname_resolv $AUDIO(cache)] \
	     [expr {$AUDIO(cachesize) * 1024}]]
    if { $AUDIO(urlcache) == "" } {
	$AUDIO(log)::warn "Could not create cache, continuing without"
    } else {
	::urlcache::geturl_opts $AUDIO(urlcache) -retries 0
    }
}

set AUDIO(servid) [::cmdserver::new $AUDIO(port) [list PLAY STOP] \
			incoming "Audio Player"]
if { $AUDIO(servid) < 0 } {
    $AUDIO(log)::critical "Could not start server on port $AUDIO(port)"
    exit
}

::cmdserver::restrict $AUDIO(servid) $AUDIO(allow) $AUDIO(deny)

vwait $AUDIO(finished)
