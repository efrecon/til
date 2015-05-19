lappend auto_path ../../tcllib1.6.1
lappend auto_path ..
package require minihttpd

::minihttpd::loglevel debug
#::massgeturl::loglevel debug
#::urlhead::loglevel debug

proc poll { } {
    global port

    puts [::minihttpd::fullurl $port "/minihttpd/httpd.tcl"]
    after 10000 poll
}

set port [::minihttpd::new [file join [file dirname $argv0] ..] -1]
after 10000 poll
vwait forever
