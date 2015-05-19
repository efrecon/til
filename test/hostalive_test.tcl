lappend auto_path ..
package require hostalive

::hostalive::loglevel debug

proc liveness { id hostname state fullname ip } {
    puts "CB: $id ($hostname) is $state"
}

set id1 [::hostalive::new doping.sics.se -alivecb liveness]
set id2 [::hostalive::new www.sics.se -alivecb liveness]

vwait forever
