lappend auto_path ../permclient
package require autoconnect

::permclient::loglevel debug
::autoconnect::loglevel debug

proc receiver { h p line } {
    puts "${h}:${p} > $line"
}

set host doping.sics.se
set port 3448
::autoconnect::receiver ${host}:${port} * ::receiver -poll 1 -block 5000
puts "[::autoconnect::get ${host}:${port} [list INFO 0] -autooff 3 -poll 1 -block 5000]"

vwait forever

